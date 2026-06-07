# ============================================================
# 3c_Populism_v4_safe.R   (BRANCH: pipeline-v4-safe)
#
# SCHEMA + PROMPT for the safe scoring pipeline.
#
# Design principles (see PIPELINE.md and PAPER_REVISIONS_PROPOSED.md
# for context):
#
#   1. Per-dimension evidence_status — absence of evidence is NOT
#      evidence against (the v1 production failure mode).
#        verified     -> explicit support/opposition in manifesto, score 0-10
#        no_evidence  -> dimension not discussed, score = null (=> NA)
#        mixed        -> pro and con both present, score 4-6
#
#   2. Anti-hallucination — prompt forbids quoting the prompt back
#      ("market-based finance, prudential oversight" was the v1 fail
#      mode). The R/12 validator double-checks every span against the
#      prompt and flags matches.
#
#   3. Verbatim quotes only, post-hoc offset assignment (as in v3).
#
#   4. Sharpened lib_financial_market — classical-/ordo-liberal, NOT
#      libertarian; regulation that ENABLES market function is liberal.
#
# This file ONLY defines schema, prompts, helpers, and a per-doc
# wrapper. It does NOT execute scoring. R/_test_v4_safe.R drives the
# small-subset test; a separate driver will do the full corpus run.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(stringi)
})
source("R/1_Functions.R")

# ---- Schema poplib_v4 ----
#
# Each scored dimension is a SCORE OBJECT { score: int|null, evidence_status, span }
# instead of a flat int. This lets the model express NA cleanly.
#
# Schema-level guarantees:
#   - score is integer 0..10 OR null (null when evidence_status="no_evidence")
#   - evidence_status in {"verified","no_evidence","mixed"}
#   - span has quote + context (no offsets — assigned post-hoc)

.int10  <- function() list(type = c("integer", "null"), minimum = 0, maximum = 10)
.int100 <- function() list(type = "integer", minimum = 0, maximum = 100)

.span_obj_v4 <- function() list(
  type = "object",
  properties = list(
    quote   = list(type = "string"),
    context = list(type = "string")
  ),
  required = c("quote", "context"),
  additionalProperties = FALSE
)

.dim_obj_v4 <- function() list(
  type = "object",
  properties = list(
    score           = .int10(),
    evidence_status = list(type = "string", enum = list("verified", "no_evidence", "mixed")),
    span            = .span_obj_v4()
  ),
  required = c("score", "evidence_status", "span"),
  additionalProperties = FALSE
)

# Some dimensions still take an ARRAY of spans (for key-phrases that summarize a
# whole category). Those use the simple span_obj_v4 wrapper.
.span_arr_v4 <- function(no) list(
  type = "array", minItems = no, maxItems = no, items = .span_obj_v4()
)

poplib_v4_schema <- list(
  type = "object",
  properties = list(
    schema_version = list(type = "string", enum = list("poplib_v4")),
    doc_id         = list(type = "string"),

    # Populism dimensions
    pop_anti_elitism     = .dim_obj_v4(),
    pop_people_centrism  = .dim_obj_v4(),
    pop_manichean        = .dim_obj_v4(),
    populism_overall     = .dim_obj_v4(),

    # Populist ideology
    pop_ideology_left     = .dim_obj_v4(),
    pop_ideology_right    = .dim_obj_v4(),
    pop_ideology_centrist = .dim_obj_v4(),
    pop_ideology_overall  = .dim_obj_v4(),

    # Liberalism (4 sub + overall)
    lib_political         = .dim_obj_v4(),
    lib_social            = .dim_obj_v4(),
    lib_economic          = .dim_obj_v4(),
    lib_financial_market  = .dim_obj_v4(),
    liberalism_overall    = .dim_obj_v4(),

    # Justifications
    pop_justification          = list(type = "string"),
    pop_ideology_justification = list(type = "string"),
    lib_justification          = list(type = "string"),

    # Confidence
    populism_confidence       = .int100(),
    pop_ideology_confidence   = .int100(),
    liberalism_confidence     = .int100()
  ),
  required = c(
    "schema_version", "doc_id",
    "pop_anti_elitism", "pop_people_centrism", "pop_manichean", "populism_overall",
    "pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist", "pop_ideology_overall",
    "lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall",
    "pop_justification", "pop_ideology_justification", "lib_justification",
    "populism_confidence", "pop_ideology_confidence", "liberalism_confidence"
  ),
  additionalProperties = FALSE
)

# ---- v4 system prompt ----
#
# Three things v4 says explicitly that v1/v2/v3 do not:
#   (1) "Absence of evidence is NOT evidence against — return NA"
#   (2) "Do NOT cite category definitions as evidence — anti-hallucination"
#   (3) "Spans MUST mention the dimension's topic (finance/bank/capital for
#        lib_financial_market; civil rights/minorities for lib_social; etc.)"

system_prompt_v4_safe <- function(country, year) paste(
  "You are an expert political text coder. Read the whole TEXT holistically.",
  "Classify POPULISM (3 sub-dimensions + overall), POPULIST IDEOLOGY (left/right/centrist + overall),",
  "and LIBERALISM (4 sub-dimensions + overall). Return STRICT JSON matching the json_schema exactly.",
  "",
  "MULTILINGUAL & CONTEXT:",
  "- The manifesto can be in any European language (German, English, French, Spanish, Italian, etc.).",
  "- Infer meaning from context (negation, irony, stance) - do NOT rely on keywords.",
  "",
  "===============================================================",
  "*** CRITICAL SCORING RULES (these are the most important rules) ***",
  "===============================================================",
  "",
  "For EACH dimension, return: { score, evidence_status, span }",
  "",
  "evidence_status:",
  "  'verified'    -> the manifesto explicitly discusses this dimension (pro or con).",
  "                   Provide a 3-8 word VERBATIM quote from TEXT that is topically",
  "                   ABOUT the dimension (e.g., for lib_financial_market, the quote must",
  "                   mention finance, banking, capital, credit, investment, or financial",
  "                   markets; for lib_social, it must mention civil rights, minorities,",
  "                   discrimination, equality, or gender/LGBTQ+).",
  "                   Set score to a 0-10 integer reflecting the stance (10=fully pro-liberal,",
  "                   0=fully anti-liberal).",
  "",
  "  'no_evidence' -> the manifesto does NOT discuss this dimension at all.",
  "                   Set score = null (JSON null).",
  "                   Set span.quote = \"\" and span.context = \"\".",
  "                   *** ABSENCE OF EVIDENCE IS NOT EVIDENCE AGAINST. ***",
  "                   Do NOT score this dimension as 0 (anti-liberal) just because the",
  "                   manifesto is silent on it. Return null (no_evidence) instead.",
  "                   Example: a manifesto about education policy with no mention of",
  "                   banking/finance -> lib_financial_market.evidence_status='no_evidence'",
  "                   and lib_financial_market.score=null.",
  "",
  "  'mixed'       -> the manifesto contains BOTH pro- and anti-stance content for this",
  "                   dimension. Provide a 3-8 word verbatim quote illustrating the mixed",
  "                   stance. Set score in the 4-6 range.",
  "",
  "===============================================================",
  "*** ANTI-HALLUCINATION RULES — non-negotiable ***",
  "===============================================================",
  "",
  "Every quote MUST be VERBATIM TEXT from the manifesto, copied letter-for-letter.",
  "",
  "FORBIDDEN, will be flagged as a hallucination by post-processing:",
  "  - quoting the category DEFINITION as if it were evidence",
  "    (e.g., quoting 'market-based finance, prudential oversight' or 'civil rights,",
  "    minority protections' — those are MY category labels, NOT manifesto text).",
  "  - paraphrasing or summarizing the manifesto in your own words.",
  "  - quoting generic phrases like 'democracy, rule of law' unless those EXACT words",
  "    appear verbatim in the manifesto.",
  "",
  "If you cannot find a 3-8 word verbatim quote in the manifesto that is topically about",
  "the dimension, set evidence_status='no_evidence' and score=null. Do not fabricate.",
  "",
  "===============================================================",
  "DIMENSION DEFINITIONS (for scoring direction, NOT for quoting back)",
  "===============================================================",
  "",
  "POPULISM (0..10 each; POPPA-aligned):",
  "- pop_anti_elitism: elites/establishment framed as corrupt or out of touch.",
  "- pop_people_centrism: 'the people' invoked as a unified, virtuous group.",
  "- pop_manichean: moralized good-vs-evil struggle; betrayal, conspiracy.",
  "- populism_overall: rounded mean of the three subscores, +/-1 if breadth warrants.",
  "",
  "POPULIST IDEOLOGY (0..10 each):",
  "- pop_ideology_left: anti-corporate elite, redistribution, social equality.",
  "- pop_ideology_right: national/identity protectionism, anti-immigration.",
  "- pop_ideology_centrist: generic people-elite appeals without clear programmatic stance.",
  "- pop_ideology_overall: overall strength of populist ideological flavor.",
  "",
  "LIBERALISM (0..10 each). 'Liberalism' here is CLASSICAL/ORDO-LIBERAL, NOT libertarianism.",
  "Regulation that ENABLES market function is pro-liberal; regulation that SUBSTITUTES",
  "for the market is anti-liberal.",
  "",
  "- lib_political: democracy, rule of law, checks & balances, media freedom, due process,",
  "                 individual political rights.",
  "  Topic check: a verbatim quote for lib_political must mention democracy, rights,",
  "  elections, courts, media, government, parliament, or a related political-institution term.",
  "",
  "- lib_social: civil rights, minority protections, gender/LGBTQ+ equality, pluralism.",
  "  Topic check: the quote must mention rights, minorities, equality, discrimination,",
  "  gender/LGBTQ+, race, religion, or related civil-society terms.",
  "",
  "- lib_economic: free markets, competition, free enterprise, deregulation/privatization,",
  "                tax efficiency, trade openness.",
  "  Topic check: the quote must mention market, competition, business, enterprise, tax,",
  "  trade, regulation, privatization, or related economic-policy terms.",
  "",
  "- lib_financial_market: support for market-based pricing of assets and credit; open",
  "  competition in banking, insurance, and capital markets; free cross-border capital flows;",
  "  PRUDENTIAL rules that ENABLE market function (disclosure, anti-fraud, deposit insurance,",
  "  fair access for retail/small investors, property/contract enforcement); transparency.",
  "  Score LOW for: capital controls, financial-transaction taxes, nationalization of banks,",
  "  state-set interest rates, price controls on financial products.",
  "  CRITICAL: investor protection, prudential regulation, and transparency are PRO-LIBERAL",
  "  ONLY when they ENABLE market-based finance. They must not be conflated with restrictions",
  "  on market pricing or capital flows.",
  "  Topic check: the quote must mention finance, bank, capital, credit, invest, market,",
  "  monetary, interest rate, currency, debt, securities, or related financial-system terms.",
  "  If the manifesto does NOT discuss financial-market policy at all, return",
  "  evidence_status='no_evidence' and score=null.",
  "",
  "- liberalism_overall: rounded mean of the four sub-dimension scores that have",
  "  evidence_status != 'no_evidence'. If ALL four sub-dimensions are no_evidence, set",
  "  liberalism_overall.evidence_status='no_evidence' and score=null.",
  "",
  "===============================================================",
  "OUTPUT SHAPE",
  "===============================================================",
  "",
  "Each dimension is an OBJECT with three fields: { score, evidence_status, span }.",
  "score is an INTEGER (0..10) OR null. Use JSON null, not 'NA' or 0.",
  "span is { quote, context } — verbatim text + ~60-character surrounding context.",
  "",
  "JUSTIFICATIONS: pop_justification, pop_ideology_justification, lib_justification —",
  "1 crisp sentence each, grounded in the verified quotes (NOT in the prompt definitions).",
  "",
  "CONFIDENCE (0..100): High (80-100) if multiple, clear cues; Medium (40-79); Low (0-39).",
  "Confidence on a dimension that returned no_evidence should reflect confidence that the",
  "manifesto really has no relevant content (high confidence = sure it's absent).",
  "",
  paste0("META: Country=", country, "; ElectionYear=", year, ". Score solely from textual evidence.")
)

user_prompt_v4 <- function(chunk_text, doc_id) {
  paste0("DOC_ID: ", doc_id, "\nTEXT:\n", chunk_text)
}

# ---- Post-hoc helpers ----

# Extract the (score, evidence_status, quote) triple from a v4 parsed dimension.
# Returns a 1-row tibble. NA-safe.
extract_v4_dim <- function(dim_obj, dim_name) {
  if (is.null(dim_obj)) {
    return(tibble(
      !!paste0(dim_name, "_score") := NA_real_,
      !!paste0(dim_name, "_evidence_status") := NA_character_,
      !!paste0(dim_name, "_quote") := NA_character_,
      !!paste0(dim_name, "_context") := NA_character_
    ))
  }
  tibble(
    !!paste0(dim_name, "_score") := if (is.null(dim_obj$score)) NA_real_ else as.numeric(dim_obj$score),
    !!paste0(dim_name, "_evidence_status") := as.character(dim_obj$evidence_status %||% NA),
    !!paste0(dim_name, "_quote")   := as.character(dim_obj$span$quote   %||% NA),
    !!paste0(dim_name, "_context") := as.character(dim_obj$span$context %||% NA)
  )
}

# Topic-relevance keyword regexes per dimension (case-insensitive).
# Used by R/12 to flag off-topic spans. Multilingual coverage:
# EN/ES/FR/IT/PT/DE/NL/SV/DA/NO/FI/PL/CZ/SK/SL/HR/SR/RO/HU/EL/LT/LV/ET/RU/BG.
# Word stems where possible (case-insensitive matching).
TOPIC_KEYWORDS_V4 <- list(

  # POLITICAL liberalism: democracy, rule of law, rights, courts, parliament, media
  lib_political = paste0(
    "democr|demokra|демокра|δημοκρα|",
    "elect|wybor|избор|εκλογ|valim|valg|valitut|valgt|választ|alegeri|izbori|izbo|",
    "parliament|parlament|seimas|seim|riigikogu|saeima|riksdag|kongres|asamblea|congres|",
    "court|tribunal|trybunał|soud|súd|tiesa|teismas|kohus|domstol|bíróság|δικαστήρι|",
    "constitut|konstytuc|ústav|konstitut|alkotmány|",
    "rule.of.law|rechtsstaat|estado.{0,5}derecho|état.{0,5}droit|stato.{0,5}diritto|estado.{0,5}direito|Rechtsstaat|",
    "judiciar|judg|juge|juiz|juez|giudic|rechter|domar|soudc|sodišč|",
    "government|gobierno|gouvernement|governo|regier|regering|vláda|vlád|valdžia|valitsus|valdība|kormány|",
    "media|prensa|presse|stampa|imprensa|medien|sajt|massmedia|sredstva|μεσα.{0,5}ενημ|",
    "free.{0,5}speech|freedom.of.speech|liberté.{0,5}expression|wolność.{0,5}słowa|libertad.{0,5}expresión|",
    "right|derecho|droit|diritto|direito|recht|hak|jog|prawo|prava|prawa|drepturi|teisė|tiesība|õigus|δικαίωμα|право|"
  ),

  # SOCIAL liberalism: civil rights, minorities, gender/LGBTQ+, equality, pluralism
  lib_social = paste0(
    "minorit|mniejsz|menšin|małej|менши|μειονότ|",
    "equalit|igualdad|égalité|uguaglianza|igualdade|gleichheit|gelijkheid|jämlikhet|ligheit|égalité|",
    "rovnost|enakost|jednakost|rovnoprávn|", # CZ/SK/SL/HR
    "egyenlőség|", # HU
    "discrimin|dyskryminac|diskrimin|diskrim|διάκριση|",
    "gender|género|genre|genere|gênero|geschlecht|płeć|spol|nem|γένος|",
    "LGBT|gay|lesbian|queer|trans|homosex|",
    "religi|religious|religioz|religios|religijn|náboženst|vallási|vjersko|θρησκ|",
    "race|raza|race|razza|raça|rasse|etniczn|etnick|etnik|etniko|fajt|",
    "ethnic|étnico|étnique|etnico|etnico|etnisch|etnisk|etniczn|etnické|nemzetiség|",
    "pluralis|pluralism|pluralismo|pluralismus|пуралізм|",
    "tolerance|tolerancia|tolérance|tolleranza|tolerância|toleranz|tolerantie|tolerancja|tolerance|",
    "civil.{0,5}right|derecho.{0,5}civil|droit.{0,5}civil|diritto.{0,5}civile|direito.{0,5}civil|bürgerrecht|",
    "human.{0,5}right|derecho.{0,5}humano|droit.{0,5}humain|diritto.{0,5}umano|direito.{0,5}humano|menschenrecht|ανθρώπιν.{0,5}δικαίωμα|"
  ),

  # ECONOMIC liberalism: markets, competition, tax, trade, business, regulation
  lib_economic = paste0(
    "market|mercado|marché|mercato|markt|marknad|marked|markkin|rynek|trh|trg|piac|piaţ|piat|αγορά|пазар|tirg|turg|rink|",
    "business|empresa|entreprise|impresa|empresa|unternehmen|onderneming|företag|virksomh|firma|firmă|cég|επιχείρηση|biznes|verslo|uzņēmum|ettevõte|",
    "enterprise|emprend|entreprend|imprend|empreend|unternehmer|przedsiębior|podnik|vállalk|επιχειρ|",
    "competit|competenc|concurrenc|concorrenz|wettbewerb|mededing|konkur|konkuren|verseny|ανταγων|",
    "tax|impost|impôt|imposta|imposto|steuer|belasting|skatt|skat|verot|podatek|daň|adó|impozit|φόρο|данък|nodok|maks|",
    "trade|comercio|commerce|commerci|handel|handel|kauppa|handel|obchod|obchod|kereskedel|comerţ|comerc|εμπόρι|търгов|prekyb|tirdzniec|kaubandu|",
    "regulat|regulación|réglement|regolament|reglament|regulier|regule|regulac|szabályoz|regulamen|reglement|κανονισμ|",
    "privat|privatiz|privatizac|prywaty|privatiz|privatizál|приват|prīvat|erastam|",
    "deregulat|dereglament|desregul|deregul|liberalizac|",
    "free.{0,5}market|libre.{0,5}mercado|libre.{0,5}marché|libero.{0,5}mercato|freie.{0,5}markt|vrije.{0,5}markt|fria.{0,5}marknad|wolny.{0,5}rynek|свободен.{0,5}пазар|szabad.{0,5}piac|",
    "industr|przemys|průmysl|priemys|ipar|industri|βιομηχαν|промышлен|"
  ),

  # FINANCIAL-MARKET liberalism: banks, finance, credit, capital, investment, monetary, currency
  lib_financial_market = paste0(
    # banks (Latin script)
    "bank|banca|banc|banco|banque|panga|pank|pankki|banka|банк|τράπεζ|",
    # finance/financial
    "financ|finans|finanz|finanč|finanč|finanč|finanz|финанс|χρηματ|talous|talou|pénzügy|finanţa|finanç|",
    # capital
    "kapital|capital|κεφάλαι|pääoma|tőke|tikra|капитал|",
    # credit/loan
    "credit|kredit|kredyt|úvěr|úver|krediit|kredīt|kredyto|hitel|кредит|πίστωση|",
    "loan|prestit|préstamo|prêt|empréstimo|darlehen|lening|lån|laina|pożyczk|půjčk|pôžičk|kölcsön|împrumut|laen|aizdev|paskol|δάνει|заём|",
    # invest/investment
    "invest|invester|investit|investiz|invest|inwest|investic|investīc|investīcij|invester|beruház|investiție|sijoit|επένδυση|инвест|",
    # monetary / currency
    "monetar|monétaire|monetair|monetær|monetari|νόμισμα|",
    "currency|moneda|monnaie|moneta|valuta|měn|men|valuut|valiut|valoda|νομίσμα|пени|",
    # debt
    "debt|deuda|dette|debito|dívida|schuld|skuld|gæld|gjeld|velka|dług|dluh|adósság|datori|võlg|parād|skol|χρέος|дълг|",
    # securities / shares / bonds
    "securit|valor|valeur|valore|valor|wertpapier|effecte|värdepapper|værdipapir|arvopaper|papier.{0,5}wartośc|cenné.{0,5}papír|értékpapír|μετοχ|акци|",
    "stock|action|azione|ação|aktie|aktia|akcij|akcj|akci|részvény|κinit|αξί|",
    "bond|obligation|obbligazione|obrigação|anleihe|obligatie|obligation|joukkovelkakirj|obligacj|obligace|obligác|kötvény|obligaţiune|obligatsioon|obligācij|obligacij|ομόλογ|облигаци|",
    # interest rate
    "interest.{0,5}rate|tasa.{0,5}interés|taux.{0,5}intérêt|tasso.{0,5}interesse|taxa.{0,5}juros|zinssatz|rentevoet|räntan|rentesats|korko|stopa.{0,5}procentowa|úrok|kamat|τόκ|лихв|procent|palūkan|",
    "interest|interés|intérêt|interesse|juros|zins|rente|ränta|korko|úrok|kamat|τόκ|лихв|palūkan|procent|",
    # insurance
    "insur|assicur|asegur|assur|assicur|versicher|verzeker|försäkr|forsikr|vakuutus|ubezpiecz|pojišť|pojišt|poistov|biztosít|asigur|ασφάλει|застраховк|kindlust|apdroš|draudim|",
    # specific finance terms
    "hipotec|hypothec|hypothek|hypotek|hipoték|mortgage|pożyczk|jelzálog|úvěr.{0,5}hypotečn|",
    "financial.{0,5}market|mercado.{0,5}financier|marché.{0,5}financier|mercato.{0,5}finanziar|finanzmarkt|финансов.{0,5}пазар|finanšu.{0,5}tirg|"
  ),

  # POPULIST ANTI-ELITISM: elite, establishment, corruption, oligarchy
  pop_anti_elitism = paste0(
    "elite|élite|élit|élites|элит|еліт|elity|eliit|eliţ|elit|",
    "establishment|stablishment|стандарт|establishement|wirtschaftselite|",
    "corrupt|korruption|корруп|korupc|korruptsioon|korups|корупц|διαφθορ|romlott|",
    "caste|kasta|kasta|κάστα|",
    "oligarch|oligark|oligarq|oligarc|олигарх|ολιγαρχ|oligarchia|",
    "kleptocrac|kleptokra|клептокр|κλεπτοκρα|",
    "cronyism|nepot|кумовство|kumotsv|nepotyzm|nepotizmus|",
    "establishment.{0,5}politic|insider|szpitalisty|",
    "system.is.rigged|rigged.system|system.{0,5}corrupt|corruptos|",
    "out.of.touch|établi|alejados|alejada|",
    "self.{0,5}serving|self.{0,5}interest|interesados|",
    "ruling.{0,5}class|herrsch|cl[aá]ss.{0,5}dirigent|правлящ|"
  ),

  # POPULIST PEOPLE-CENTRISM: 'the people', nation, ordinary, hardworking, citizens
  pop_people_centrism = paste0(
    "the.{0,5}people|el.{0,5}pueblo|le.{0,5}peuple|il.{0,5}popolo|o.{0,5}povo|das.{0,5}volk|het.{0,5}volk|folket|narod|naród|národ|nemzet|η.{0,5}λαός|народ|tauta|tauta|rahva|tauta|",
    "the.{0,5}nation|la.{0,5}nación|la.{0,5}nation|la.{0,5}nazione|a.{0,5}nação|die.{0,5}nation|natie|naţiunea|nemzet|έθνος|",
    "popular|populac|popul|népi|λαϊκ|народни|popolare|populární|",
    "grassroots|bazą|baza.{0,5}społeczna|baza.{0,5}sociala|",
    "ordinary.{0,5}people|gente.{0,5}común|gens.{0,5}ordinaires|gente.{0,5}comune|gewone.{0,5}mensen|zwykli.{0,5}ludzi|prosti.{0,5}ľudia|обикновените.{0,5}хора|",
    "working.{0,5}people|trabajador|travailleur|lavoratori|trabalhador|arbeit|pracownicy|pracujúci|munkás|трудящ|εργαζομέν|",
    "the.{0,5}citizens|los.{0,5}ciudadanos|les.{0,5}citoyens|i.{0,5}cittadini|os.{0,5}cidadãos|die.{0,5}bürger|obywatel|občan|polgár|cidadã|građan|",
    "hardworking|trabajado|esforzado|fleißig|hart.{0,5}werkend|pracowit|munkás|"
  ),

  # POPULIST MANICHEAN: betrayal, enemies, conspiracy, war metaphors
  pop_manichean = paste0(
    "betray|traidor|traître|traditor|traidor|verraten|verraad|svikare|svik|verraad|zradc|zrad|árul|prodatej|προδ|предател|nodevě|tahkasid|",
    "traitor|trahison|verrat|verraad|verräd|zdrad|árul|προδότ|изменник|",
    "enemy|enemig|ennemi|nemico|inimigo|feind|vijand|fiende|fjende|nepriatel|nepřítel|враг|εχθρός|wróg|priešas|ienaidnieks|vaenlane|ellenség|",
    "conspirac|conspirar|complot|conspiraz|verschwör|verzwering|konspir|спіран|σύνωμοσ|tényfeltár|sokomp|",
    "treason|verrat|verraad|zdrada|zrada|árulás|predaja|",
    "war.{0,5}on|guerra.{0,5}contra|guerre.{0,5}contre|krieg.{0,5}gegen|wojna.{0,5}z|война.{0,5}против|πόλεμο.{0,5}κατά|",
    "fight.{0,5}against|lucha.{0,5}contra|lutte.{0,5}contre|lotta.{0,5}contro|luta.{0,5}contra|kampf.{0,5}gegen|harc.{0,5}ellen|боя.{0,5}срещу|αγώνας.{0,5}κατά|kova.{0,5}su|cīnīsies|",
    "battle|batalla|bataille|battaglia|kampf|bitwa|harc|μάχ|битка|",
    "deep.{0,5}state|estado.{0,5}profundo|état.{0,5}profond|stato.{0,5}profondo|tiefer.{0,5}staat|głębok.{0,5}państw|βαθύ.{0,5}κράτος|",
    "globalist|globalista|globaliste|globalist|globalist|globalizm|globalna|παγκοσμιοκράτ|глобалист|",
    "invasion|invasión|invasion|invasione|invasão|инвазия|"
  )
)

message("R/3c_Populism_v4_safe.R loaded: poplib_v4_schema, system_prompt_v4_safe(), TOPIC_KEYWORDS_V4.")
