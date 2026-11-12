# CODEBOOK — populism, ideology and liberalism scores (v2026a)

Everything here is taken from the code: the prompt and schema from `R/3c_Populism_v4_safe.R`, the
request settings from `R/15b`, `R/16`, `R/18` and `R/32`, the validation and aggregation from
`R/34_Span_Checks_v2026a.R` and `R/1_Aggregation.R`, and the variables from the files themselves.

## 1. Unit, corpus, chunking

- **Corpus**: MPDS2026a / Manifesto Corpus 2026-1 (`20260904101355`); 3,371 manifestos with text
  (`data/CMP/full_data_2026a_meta.parquet`, `has_text`).
  - The text is the corpus content with quasi-sentences joined by a space, `•` removed and runs of
    whitespace collapsed (`R/2_Helpers.R::clean_manifesto_text`).
- **Chunking** (`R/1_Functions.R`, constants in `R/15b`/`R/16`/`R/18`):
  - Token counts use `rtiktoken`'s o200k tokenizer (`model = "gpt-4o-mini"`).
  - A manifesto of at most 18,000 tokens is sent whole.
  - Longer ones are split into chunks of 20,000 tokens with a 2,000-token overlap.
  - If the tokenizer cannot decode a split point (one document, 89325_202405, 84k tokens), the whole
    text is sent as one chunk.
  - Chunk ids: `<doc_id>_ch<k>`.
- **Unit of scoring**: one chunk × one model × 13 dimensions.

## 2. Models and request settings

| Family | Model requested | Endpoint | Settings | Resolved version |
|---|---|---|---|---|
| Anthropic | `claude-sonnet-4-6` (an undated alias, not a dated snapshot) | Messages API, forced tool use (`submit_poplib_v4`) | temperature 0, max_tokens 5000, no top_p; schema with nullable types stripped (`anthropic_v4_schema`) | logged per call for every 2026-10 row (`model_resolved`); not logged in June |
| OpenAI | `gpt-4.1-mini-2025-04-14` (dated snapshot) | Responses API, strict JSON schema | temperature 0, top_p 1, store TRUE, max_output_tokens 5000 | as above |
| Google | June: `gemini-flash-latest` (floating alias, AI Studio); 2026-10: `gemini-3.5-flash` (Vertex AI `global`; AI Studio for the 83 update docs) | generateContent, responseSchema | temperature 0, maxOutputTokens 12000, thinkingBudget 0, no top_p | June: **not logged, inferred** `gemini-3.5-flash` from an anchor test (`data/CMP/v2026a/ANCHOR.md`); 2026-10: logged |
| Judge (OpenAI) | `gpt-4o-mini-2024-07-18` | Responses API, `{answer: yes/no}` | temperature 0, top_p 1, max_output_tokens 20 | logged per call (`judge_topic_v2026a.parquet`) |

The shares of chunks per model, endpoint and provenance status (logged / inferred / not logged)
are in `output/v2026a/csv/40_model_provenance.csv`. Per manifesto, the baseline carries
`endpoint_<model>`, `resolved_<model>` and `share_logged_<model>`. The settings differ by family;
there is no single configuration for all models.

## 3. System prompt (verbatim)

`system_prompt_v4_safe(country, year)` builds the prompt with `paste()` and its default separator.
The 123 elements below are therefore joined by **single spaces**, and the models receive **one
line** of 7,224 characters (plus the country and year). The line breaks and empty elements in the
source are not transmitted as line breaks. md5 of the prompt for `country = "Germany", year = 2025`:
`b5d91a8ddde492951830e6372a51159d`. The elements, one per line, exactly as in the source (`{country}`
and `{year}` are substituted per manifesto from the CMP metadata):

```text
You are an expert political text coder. Read the whole TEXT holistically.
Classify POPULISM (3 sub-dimensions + overall), POPULIST IDEOLOGY (left/right/centrist + overall),
and LIBERALISM (4 sub-dimensions + overall). Return STRICT JSON matching the json_schema exactly.

MULTILINGUAL & CONTEXT:
- The manifesto can be in any European language (German, English, French, Spanish, Italian, etc.).
- Infer meaning from context (negation, irony, stance) - do NOT rely on keywords.

===============================================================
*** CRITICAL SCORING RULES (these are the most important rules) ***
===============================================================

For EACH dimension, return: { score, evidence_status, span }

evidence_status:
  'verified'    -> the manifesto explicitly discusses this dimension (pro or con).
                   Provide a 3-8 word VERBATIM quote from TEXT that is topically
                   ABOUT the dimension (e.g., for lib_financial_market, the quote must
                   mention finance, banking, capital, credit, investment, or financial
                   markets; for lib_social, it must mention civil rights, minorities,
                   discrimination, equality, or gender/LGBTQ+).
                   Set score to a 0-10 integer reflecting the stance (10=fully pro-liberal,
                   0=fully anti-liberal).

  'no_evidence' -> the manifesto does NOT discuss this dimension at all.
                   Set score = null (JSON null).
                   Set span.quote = "" and span.context = "".
                   *** ABSENCE OF EVIDENCE IS NOT EVIDENCE AGAINST. ***
                   Do NOT score this dimension as 0 (anti-liberal) just because the
                   manifesto is silent on it. Return null (no_evidence) instead.
                   Example: a manifesto about education policy with no mention of
                   banking/finance -> lib_financial_market.evidence_status='no_evidence'
                   and lib_financial_market.score=null.

  'mixed'       -> the manifesto contains BOTH pro- and anti-stance content for this
                   dimension. Provide a 3-8 word verbatim quote illustrating the mixed
                   stance. Set score in the 4-6 range.

===============================================================
*** ANTI-HALLUCINATION RULES — non-negotiable ***
===============================================================

Every quote MUST be VERBATIM TEXT from the manifesto, copied letter-for-letter.

FORBIDDEN, will be flagged as a hallucination by post-processing:
  - quoting the category DEFINITION as if it were evidence
    (e.g., quoting 'market-based finance, prudential oversight' or 'civil rights,
    minority protections' — those are MY category labels, NOT manifesto text).
  - paraphrasing or summarizing the manifesto in your own words.
  - quoting generic phrases like 'democracy, rule of law' unless those EXACT words
    appear verbatim in the manifesto.

If you cannot find a 3-8 word verbatim quote in the manifesto that is topically about
the dimension, set evidence_status='no_evidence' and score=null. Do not fabricate.

===============================================================
DIMENSION DEFINITIONS (for scoring direction, NOT for quoting back)
===============================================================

POPULISM (0..10 each; POPPA-aligned):
- pop_anti_elitism: elites/establishment framed as corrupt or out of touch.
- pop_people_centrism: 'the people' invoked as a unified, virtuous group.
- pop_manichean: moralized good-vs-evil struggle; betrayal, conspiracy.
- populism_overall: rounded mean of the three subscores, +/-1 if breadth warrants.

POPULIST IDEOLOGY (0..10 each):
- pop_ideology_left: anti-corporate elite, redistribution, social equality.
- pop_ideology_right: national/identity protectionism, anti-immigration.
- pop_ideology_centrist: generic people-elite appeals without clear programmatic stance.
- pop_ideology_overall: overall strength of populist ideological flavor.

LIBERALISM (0..10 each). 'Liberalism' here is CLASSICAL/ORDO-LIBERAL, NOT libertarianism.
Regulation that ENABLES market function is pro-liberal; regulation that SUBSTITUTES
for the market is anti-liberal.

- lib_political: democracy, rule of law, checks & balances, media freedom, due process,
                 individual political rights.
  Topic check: a verbatim quote for lib_political must mention democracy, rights,
  elections, courts, media, government, parliament, or a related political-institution term.

- lib_social: civil rights, minority protections, gender/LGBTQ+ equality, pluralism.
  Topic check: the quote must mention rights, minorities, equality, discrimination,
  gender/LGBTQ+, race, religion, or related civil-society terms.

- lib_economic: free markets, competition, free enterprise, deregulation/privatization,
                tax efficiency, trade openness.
  Topic check: the quote must mention market, competition, business, enterprise, tax,
  trade, regulation, privatization, or related economic-policy terms.

- lib_financial_market: support for market-based pricing of assets and credit; open
  competition in banking, insurance, and capital markets; free cross-border capital flows;
  PRUDENTIAL rules that ENABLE market function (disclosure, anti-fraud, deposit insurance,
  fair access for retail/small investors, property/contract enforcement); transparency.
  Score LOW for: capital controls, financial-transaction taxes, nationalization of banks,
  state-set interest rates, price controls on financial products.
  CRITICAL: investor protection, prudential regulation, and transparency are PRO-LIBERAL
  ONLY when they ENABLE market-based finance. They must not be conflated with restrictions
  on market pricing or capital flows.
  Topic check: the quote must mention finance, bank, capital, credit, invest, market,
  monetary, interest rate, currency, debt, securities, or related financial-system terms.
  If the manifesto does NOT discuss financial-market policy at all, return
  evidence_status='no_evidence' and score=null.

- liberalism_overall: rounded mean of the four sub-dimension scores that have
  evidence_status != 'no_evidence'. If ALL four sub-dimensions are no_evidence, set
  liberalism_overall.evidence_status='no_evidence' and score=null.

===============================================================
OUTPUT SHAPE
===============================================================

Each dimension is an OBJECT with three fields: { score, evidence_status, span }.
score is an INTEGER (0..10) OR null. Use JSON null, not 'NA' or 0.
span is { quote, context } — verbatim text + ~60-character surrounding context.

JUSTIFICATIONS: pop_justification, pop_ideology_justification, lib_justification —
1 crisp sentence each, grounded in the verified quotes (NOT in the prompt definitions).

CONFIDENCE (0..100): High (80-100) if multiple, clear cues; Medium (40-79); Low (0-39).
Confidence on a dimension that returned no_evidence should reflect confidence that the
manifesto really has no relevant content (high confidence = sure it's absent).

META: Country={country}; ElectionYear={year}. Score solely from textual evidence.
```

User message (`user_prompt_v4`):

```text
DOC_ID: {doc_id}
TEXT:
{chunk text}
```

## 4. Output schema `poplib_v4` (verbatim, `poplib_v4_schema`)

Claude receives the same schema with `["integer","null"]` replaced by `"integer"` (Anthropic tool
schemas do not accept union types). Gemini receives it converted by `to_gemini_schema()`.

```json
{
  "type": "object",
  "properties": {
    "schema_version": {
      "type": "string",
      "enum": [
        "poplib_v4"
      ]
    },
    "doc_id": {
      "type": "string"
    },
    "pop_anti_elitism": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_people_centrism": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_manichean": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "populism_overall": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_ideology_left": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_ideology_right": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_ideology_centrist": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_ideology_overall": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "lib_political": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "lib_social": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "lib_economic": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "lib_financial_market": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "liberalism_overall": {
      "type": "object",
      "properties": {
        "score": {
          "type": ["integer", "null"],
          "minimum": 0,
          "maximum": 10
        },
        "evidence_status": {
          "type": "string",
          "enum": [
            "verified",
            "no_evidence",
            "mixed"
          ]
        },
        "span": {
          "type": "object",
          "properties": {
            "quote": {
              "type": "string"
            },
            "context": {
              "type": "string"
            }
          },
          "required": ["quote", "context"],
          "additionalProperties": false
        }
      },
      "required": ["score", "evidence_status", "span"],
      "additionalProperties": false
    },
    "pop_justification": {
      "type": "string"
    },
    "pop_ideology_justification": {
      "type": "string"
    },
    "lib_justification": {
      "type": "string"
    },
    "populism_confidence": {
      "type": "integer",
      "minimum": 0,
      "maximum": 100
    },
    "pop_ideology_confidence": {
      "type": "integer",
      "minimum": 0,
      "maximum": 100
    },
    "liberalism_confidence": {
      "type": "integer",
      "minimum": 0,
      "maximum": 100
    }
  },
  "required": ["schema_version", "doc_id", "pop_anti_elitism", "pop_people_centrism", "pop_manichean", "populism_overall", "pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist", "pop_ideology_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall", "pop_justification", "pop_ideology_justification", "lib_justification", "populism_confidence", "pop_ideology_confidence", "liberalism_confidence"],
  "additionalProperties": false
}
```

## 5. Dimensions

| Block | Dimension | Definition in the prompt | Confidence block |
|---|---|---|---|
| Populism | `pop_anti_elitism` | elites/establishment framed as corrupt or out of touch | `populism_confidence` |
| | `pop_people_centrism` | "the people" invoked as a unified, virtuous group | `populism_confidence` |
| | `pop_manichean` | moralized good-vs-evil struggle; betrayal, conspiracy | `populism_confidence` |
| | `populism_overall` | rounded mean of the three subscores, ±1 if breadth warrants | `populism_confidence` |
| Populist ideology | `pop_ideology_left` | anti-corporate elite, redistribution, social equality | `pop_ideology_confidence` |
| | `pop_ideology_right` | national/identity protectionism, anti-immigration | `pop_ideology_confidence` |
| | `pop_ideology_centrist` | generic people-elite appeals without clear programmatic stance | `pop_ideology_confidence` |
| | `pop_ideology_overall` | overall strength of populist ideological flavor | `pop_ideology_confidence` |
| Liberalism | `lib_political`, `lib_social`, `lib_economic`, `lib_financial_market`, `liberalism_overall` | see the prompt, section "LIBERALISM" | `liberalism_confidence` |

Each dimension has `score` ∈ {0,…,10} or null, `evidence_status` ∈ {verified, mixed, no_evidence},
and `span` = {quote, context}.

The L/R/C subscores measure host-ideology issue content, not a property of the populism itself
(`output/v2026a/IDEOLOGY_DIAGNOSIS.md`, section 4).

## 6. Span checks (R/34; flags only, never modify a score)

For a span $s$ (quote $q$, context $c$) of dimension $d$ in chunk $k$:

- **Quote check (the filter):** $\text{quote\_ok}(s) = \text{verbatim}(q, \text{chunk}_k) \wedge \neg\,\text{in\_prompt}(q)$.
  - `verbatim`: tiered match of $q$ in the chunk text: exact, then case-insensitive, then
    whitespace- and quote-glyph-normalised (`R/12::verbatim_check`).
  - `in_prompt`: the normalised quote has at least 3 words and is a substring of the system prompt
    (`R/12::hallucination_check`).
  - Stored as `verbatim_found` and `hallucination_flag` in `validated_spans_<m>.parquet`.
- **Topic check (diagnostic and spec-curve fork, not a filter):** $\text{topic\_ok}(s) = \text{rx}_d(q \Vert c) \vee \text{judge}_d(q)$.
  - $\text{rx}_d$: the dimension's keyword regex from `TOPIC_KEYWORDS_V4`, repaired. Every pattern
    in R/3c ends in `|`, an empty alternative that matches any text, so the stored R/12 topic flag
    is always TRUE. R/34 strips the trailing `|` (`KW_FIXED`).
  - `populism_overall` and `liberalism_overall` use the union of their sub-dimension keyword sets.
    The ideology dimensions have no keywords.
  - $\text{judge}_d$: the gpt-4o-mini yes/no topic judge, asked only when the regex fails. L/R/C
    reuse the stored R/12 answer where one exists.
- Validation rules (`R/1_Aggregation.R::VALIDATION_RULES`), by dimension group (core 7 / L-R-C /
  `*_overall`):
  - `quote` (baseline): quote / quote / quote;
  - `r12`: quote / quote+topic / quote+topic;
  - `full`: quote+topic everywhere;
  - `none`: no check.

## 7. Aggregation (R/1_Aggregation.R, the only aggregation code path)

Let $s_{k,d}^{(m)}$ be the score of model $m$ for dimension $d$ in chunk $k$ of manifesto $i$, with
status $e_{k,d}^{(m)}$ and confidence $c_{k,b(d)}^{(m)}$. Here $b(d)$ is the dimension's own
confidence block (populism / pop_ideology / liberalism). The set of chunk-dims that enter is

$$\mathcal{K}_{i,d}^{(m)} = \{k \in i : e_{k,d}^{(m)} = \text{verified},\; s_{k,d}^{(m)} \neq \text{NA},\; \text{quote\_ok}(\text{span}_{k,d}^{(m)})\}.$$

Within model (confidence-weighted mean; `cwm`):

$$\bar s_{i,d}^{(m)} = \frac{\sum_{k \in \mathcal{K}} w_{k,d}^{(m)}\, s_{k,d}^{(m)}}{\sum_{k \in \mathcal{K}} w_{k,d}^{(m)}}, \qquad w_{k,d}^{(m)} = \max\!\big(c_{k,b(d)}^{(m)},\, 0.1\big)\ \text{(0–100 scale)},$$

and NA if $\mathcal{K}$ is empty. Across models, a plain mean over the models with a non-missing
score. Gap legs, i.e. a model whose leg is incomplete, are NA:

$$S_{i,d} = \frac{1}{|M_{i,d}|} \sum_{m \in M_{i,d}} \bar s_{i,d}^{(m)}.$$

Ideology (IDEO) on the cross-model means of the right and left ideology scores, NA if the
denominator is 0 or a part is missing. Since rev3, decided by Sebastian on 2026-10-27
(PLLM-0132 / PLLM-0048):

$$\text{IDEO}_i = \frac{S_{i,R} - S_{i,L}}{S_{i,R} + S_{i,L}} \in [-1, 1].$$

Until rev2 the centrist score was part of the denominator. That construction is kept as the
baseline column `pop_ideology_direction_c` (NA if any of R, L, C is missing):

$$\text{IDEO}^{c}_i = \frac{S_{i,R} - S_{i,L}}{S_{i,R} + S_{i,L} + S_{i,C}}.$$

Selection forks for IDEO (the baseline IDEO is NA if R or L is missing; R and L can rest on
different model subsets $M_{i,d}$):
- $\text{IDEO}^{0}_i$: missing parts set to 0 where $S_{i,\text{POP}}$ exists (NA only if
  $R+L=0$);
- $\text{IDEO}^{\ge 2}_i$: IDEO where $\min_{d \in \{R,L\}} |M_{i,d}| \ge 2$;
- $\text{IDEO}^{3}_i$: IDEO where all three models contribute to R and L.

Alternative constructions of the ideology direction (computed in `R/45_OA_Ideology.R` for the
online appendix, OA2; not used by any main-text table):
- $\text{IDEO}^{c}_i$ (above): the construction until rev2, with the centrist score in the
  denominator;
- $\text{IDEO}^{diff}_i = (S_{i,R} - S_{i,L}) / 10 \in [-1, 1]$: the plain difference, rescaled;
- $C^{share}_i = S_{i,C} / (S_{i,R} + S_{i,L} + S_{i,C})$, the centrist share of the denominator,
  and the shrink factor $(S_{i,R} + S_{i,L}) / (S_{i,R} + S_{i,L} + S_{i,C}) = 1 - C^{share}_i$.
  Because $\text{IDEO}^{c}_i = \text{IDEO}_i \times (1 - C^{share}_i)$, the centrist score never
  changes the sign; it only shrinks the measure towards 0.

Evidence-quote keyword classes (R/45, OA3; a diagnostic, not a coding instrument). Each verified
chunk-level span of `pop_ideology_right` with score $\ge 1$ (all three models) is classified by two
multilingual keyword regexes (identity / immigration: immigration, asylum, refugees, Islam, borders,
nation, sovereignty, homeland, culture, tradition, multiculturalism, deportation, …; economic /
trade: tariffs, trade, imports / exports, globalisation, offshoring, domestic or national industry,
"buy national", protection of jobs / farmers / industry, WTO / TTIP / CETA / Mercosur, free trade,
subsidies, agriculture, …) into *identity/immigration only*, *economic/trade only*, *both* or
*neither*, once on the quote and once on quote + context; a third regex flags `protect` words. The
regexes are in R/45 (block B5). Only counts and shares are written; the classification needs the
full quotes and contexts, so it is not reproducible from the public inputs.

Named rules (`AGG_RULES`): `canonical` = cwm with own-block weights and the quote check (the
baseline); `june` = cwm_pop without span check (the June 2026 baseline).

Chunk overlap: consecutive chunks of a long manifesto overlap by 2,000 tokens. Text in an overlap
can therefore contribute to two chunk scores. Chunks are weighted by confidence, not by length or
chunk count; no correction for the overlap is applied. 2,066 of the 3,371 manifestos (61.3%) are
a single chunk, and the longest has 26 chunks (`data/CMP/v2026a/coverage.parquet`, `n_chunks_ok`,
Claude leg).

Spec-curve alternatives:
- within model: `cwm_pop` (populism confidence for every dimension, the June rule), `mean`, `median`;
- between models: `median`, or leave-worst-out. Leave-worst-out drops the model farthest from the
  mean; with two models, or a tie, it returns the mean.
- span validation: `none`, `r12`, `full`;
- excluding the 450 Claude dims decoded by the lenient parser (`parse_repaired`); this fork runs at
  the baseline measurement path only.

## 8. Variable dictionary

### `data/CMP/v2026a/manifesto_scores_v2026a.parquet` (one row per manifesto; 3,371 × 99)

| Variable(s) | Meaning |
|---|---|
| `doc_id` | CMP manifesto id `<party>_<YYYYMM>` |
| `countryname`, `partyname`, `partyabbrev`, `party`, `parfam`, `rile`, `date`, `year`, `token_count` | MPDS2026a metadata. `party` = CMP party code (used for party FE and clustering); `token_count` = 0.75 × words (not the chunking tokenizer) |
| `pop_anti_elitism` … `liberalism_overall` (13) | baseline scores $S_{i,d}$, 0–10 |
| `pop_ideology_direction` | ideology, IDEO = (R − L) / (R + L), −1 (left) to +1 (right) (rev3; section 7) |
| `pop_ideology_direction_c` | alternative IDEO$^c$ = (R − L) / (R + L + C), the construction until rev2 (section 7) |
| `pop_ideology_direction_zero`, `_ge2`, `_all3` | the IDEO selection forks (section 7) |
| `<dim>_sonnet`, `<dim>_gpt41mini`, `<dim>_gemini` (39) | per-model scores $\bar s_{i,d}^{(m)}$ |
| `n_models_<dim>` (13) | number of models with a score for that dimension |
| `model_legs`, `n_model_legs` | models whose leg is complete (`coverage.parquet`) |
| `endpoint_<model>`, `resolved_<model>`, `share_logged_<model>` | endpoint(s) of the leg's chunks; resolved model version(s) (`not logged` for June Claude/GPT, `gemini-3.5-flash (inferred)` for June Gemini); share of chunks with a logged version |
| `source_run_<model>` | scoring run(s) of the leg's chunks (values: see `source_run` in the raw-file table below) |
| `update_status`, `text_change_type` | change relative to corpus 2025-1 (`R/30`) |
| `baseline_version`, `corpus_version`, `aggregation`, `validation_rule` | provenance stamps (`v2026a`, corpus tag, rule text, `quote`) |

### Variables derived in `R/45_OA_Ideology.R` (online appendix; not stored)

| Variable | Definition |
|---|---|
| `R`, `L`, `C` | `pop_ideology_right`, `pop_ideology_left`, `pop_ideology_centrist` (cross-model means) |
| `IDEO` | `pop_ideology_direction` $= (R - L) / (R + L)$ |
| `IDEO_c` | `pop_ideology_direction_c` $= (R - L) / (R + L + C)$ |
| `IDEO_diff` | $(R - L) / 10$ |
| `C_share` | $C / (R + L + C)$ |
| `ABS` | ideological extremity $|\text{IDEO}|$ (OA2, `tab:oa2e`) |
| `L_ext`, `R_ext` | left and right extremity $\max(-\text{IDEO}, 0)$ and $\max(\text{IDEO}, 0)$; $\text{ABS} = L_{ext} + R_{ext}$, $\text{IDEO} = R_{ext} - L_{ext}$ |
| `IDEO2` | $\text{IDEO}^2$ (quadratic alternative to ABS, csv only) |
| `shrink` | $(R + L) / (R + L + C)$ |
| `per401` … `per414`, `per601` … `per608`, `per107`, `per109`, `per110` | MPDS2026a category shares (percent of quasi-sentences), merged on `party` and `date_num` = YYYYMM, one MPDS row per (party, date_num) |
| `cls_q`, `cls_c` | keyword class of the `pop_ideology_right` evidence quote / quote + context (section 7) |

### Online-appendix selection rules (R/47)

- **English-language manifestos:** United Kingdom, Ireland, Australia, New Zealand, United States.
- **OA9 worked example (per dimension):** among chunks of English-language manifestos where all three
  models give a verified score of at least 7 with a quote that passes the quote check, the chunk with
  the smallest cross-model range (ties: higher mean score, then chunk id). The evidence printed is
  that of the model with the shortest context (ties: Sonnet, GPT-4.1-mini, Gemini).
- **OA10 model differences:** chunks of English-language manifestos with a verified score from all
  three models on `populism_overall` (resp. `liberalism_overall`); the five with the largest range
  (ties: chunk id).

### External indicators (`data/external/`, R/44a; matched in R/44)

| File | Variables used | Match |
|---|---|---|
| `ches_1999-2024_meansV2.csv` | `lrgen`, `lrecon`, `galtan` | CHES `cmp_id`, nearest wave ±2 years |
| `parlgov_view_party.csv` | `left_right` | ParlGov `cmp`, party mean |
| `poppa_2018_party_means.tab` (POPPA 2018, doi:10.7910/DVN/8NEL7B) | `populism`, `lroverall`, `lrecon` | Partyfacts (`poppa` → `manifesto`), fallback POPPA `cmp_id`; each party's election nearest to 2018, ±2 years |
| `gps_2019_party_v2_1.tab` (Global Party Survey 2019, doi:10.7910/DVN/WMGTNS) | `V8_Scale` (populist rhetoric), `V4_Scale` (economic left-right), `V6_Scale` (social conservatism) | Partyfacts (`gps` → `manifesto`); nearest election to 2019, ±2 years |
| `rfpopi_final_dataset.csv` (RFPOPI, Celico, Rode & Rodríguez Carreño 2024, EJPE, doi:10.1016/j.ejpoleco.2024.102529; static copy from GitHub acelico/RFPOPI-Populism-indicators, commit 2870ebb94df9, 2024-02-16, of the dataset offered by the Shiny app; research and non-profit use with citation) | `populism_rhet` (rhetorical), `populism_ide` (ideational), 0–10, higher = more populist; party-election level | `pf_party_id` (Partyfacts) → CMP party code; per manifesto the RFPOPI row of the same election year, otherwise the nearest within ±1 year (ties: earlier year) |

### `data/CMP/v2026a/raw_<model>.parquet` (one row per chunk; Claude: `raw_claude_rev2.parquet`)

| Variable(s) | Meaning |
|---|---|
| `doc_id`, `doc_id_root`, `chunk_id` | chunk id, manifesto id, chunk number |
| `schema_version` | `poplib_v4` |
| `<dim>_score`, `<dim>_status`, `<dim>_quote`, `<dim>_context` (13 × 4) | model output per dimension. Status NA without a scoring error = parse loss (none remain in rev2) |
| `populism_confidence`, `pop_ideology_confidence`, `liberalism_confidence` | 0–100 |
| `pop_justification`, `pop_ideology_justification`, `lib_justification` | one-sentence justifications (model text) |
| `scoring_error` | NA in the canonical files (error rows are archived) |
| `scored_at` | timestamp of the scoring call or checkpoint |
| `orig_doc_id` | chunk id under which the row was scored (party re-codes) |
| `source_run` | `june2026`, `update_2026-10`, `norm_2026-10`, `vertex_2026-10`, `retry_2026-10`, `parseloss_2026-10` |
| `scoring_target`, `endpoint` | scoring target in R/32; `anthropic-api` / `openai-api` / `aistudio` / `vertex-global` |
| `model_requested`, `model_resolved` | requested id and the version the API reported (NA for June Claude/GPT rows; `gemini-3.5-flash (inferred)` for June Gemini) |
| `corpus_version` | text version the chunk was scored on |
| `parse_repaired` (Claude rev2) | dimensions decoded from a stringified JSON object by the patched flattener |
| `raw_tool_json` (Claude rev2, re-scored rows) | the raw tool_use input, as returned |

### `validated_spans_<model>.parquet` (R/12) and `span_checks_<set>.parquet` (R/34)

- **validated_spans**: one row per (chunk, dimension) with a non-empty quote. Columns:
  - `quote`, `context`;
  - `hallucination_flag`, `verbatim_found`, `verbatim_method`, `start_in_doc`, `end_in_doc`;
  - `topic_flag` (always TRUE for core dims, see section 6);
  - `quality_verified`;
  - `judge_used`, `judge_topic`;
  - `span_source`.
- **span_checks**: no text. Columns: `doc_id`, `doc_id_root`, `dim_name`, `quote_ok`, `topic_rx`,
  `judge`, `judge_source`, `topic_ok`, `r12_quality_verified`, `span_source`.

### Other inputs

| File | Content |
|---|---|
| `coverage.parquet` | manifesto × model: chunks scored, sources, `covered`, `gap_reason` |
| `gap_chunks.parquet` | the failed chunks of the four documented gap legs, with error class |
| `judge_topic_v2026a.parquet` | one row per (dimension, quote): `judge_topic`, `model_requested`, `model_resolved`, tokens, `judged_at` |
| `archive/` | superseded and dropped rows with `drop_reason` (see `CHANGELOG_DATA.md`) |
