# Populism-LLM — replication code

Reproduction code for **Rode, M. & Stöckl, S. (2026). _The Milei question, or:
How liberal are right-wing populist parties? A measurement approach using
large language models._** (Working paper.)

- **Dataset** (HuggingFace): <https://huggingface.co/datasets/sstoeckl/populism-llm>
- **Interactive inspection site**: <https://sstoeckl.github.io/populism-llm/>
- **Companion website source**: <https://github.com/sstoeckl/populism-llm>

The pipeline scores 3,327 European party manifestos (CMP / WZB) on 12 dimensions
of populism and liberalism using three independent LLM families (Claude
Sonnet 4.6, GPT-4.1-mini, Gemini Flash) under a strict JSON schema with
verbatim-quote evidence.

---

## What you reproduce

By running the scripts in order you reproduce:

1. The cleaned CMP manifesto corpus with manifesto texts.
2. The per-chunk LLM scores from all three model families.
3. The post-hoc span-validation step (regex + LLM-as-judge for topic relevance).
4. The aggregated per-manifesto scores per model.
5. The cross-LLM aggregated table used in the headline regressions.
6. The 8,100-cell specification-curve grid that underpins Appendix C of the paper.
7. The headline figure (`spec_curve.pdf`) and per-fork CSV.
8. The regression tables reported in the main paper.

**Approximate compute budget (full pipeline from scratch):**

| Step | Wall-clock | API cost |
|---|---:|---:|
| CMP load + preprocess | 5 min | — |
| Sonnet scoring (3,327 mfsto × 4 workers) | ~6 h | ~€620 |
| GPT-4.1-mini scoring | ~3 h | ~€42 |
| Gemini Flash scoring | ~3 h | ~€31 |
| Span validation incl. LLM-as-judge | ~5 h | ~€3 |
| Spec-curve compute | ~5 min | — |
| **Total** | **~17 h** | **~€700** |

If you want to skip the LLM scoring (which is the expensive part), the
HuggingFace dataset has the raw per-chunk scores and verbatim quotes —
download them and start at `R/12_Validate_Span_Quality.R`.

---

## Setup

### 1. Clone

```bash
git clone https://github.com/sstoeckl/populism-llm-replication.git
cd populism-llm-replication
```

### 2. R packages

R ≥ 4.4 required. Install:

```r
install.packages(c(
  "tidyverse", "arrow", "httr", "jsonlite", "furrr",
  "rtiktoken",       # token counting compatible with OpenAI tokenizer
  "manifestoR",      # CMP data access
  "lubridate", "stringi", "readxl", "readr",
  "fixest",          # panel regressions for spec curve
  "ggplot2", "scales", "RColorBrewer"
))
```

### 3. API keys

Copy `.Renviron.example` to `~/.Renviron` (or to the project root) and fill in:

```
MANIFESTO_KEY=your-cmp-api-key       # from https://manifesto-project.wzb.eu/
OPENAI_API_KEY=sk-...                # OpenAI
ANTHROPIC_API_KEY=sk-ant-...         # Anthropic Console
GEMINI_API_KEY=AIza...               # Google AI Studio
```

The Manifesto Project key is free with academic registration. The three
LLM keys require paid accounts (cost estimate above).

### 4. Working directory

All scripts assume the project root as working directory. They run cleanly
from RStudio if you open the `.Rproj` file, or via `Rscript` from the project
root.

---

## Pipeline (run in this order)

| Script | Purpose | Wall-clock |
|---|---|---:|
| `R/1_Functions.R` | Utility functions, chunking, OpenAI helpers | source first |
| `R/1_batch.R` | (Optional) OpenAI Batch API helpers | source if using |
| `R/2_Manifesto_Prep.R` | Load CMP corpus → `data/CMP/full_data.parquet` | 5 min |
| `R/3c_Populism_v4_safe.R` | Defines `poplib_v4_schema` + `system_prompt_v4_safe()` + `user_prompt_v4()`. Loaded by every scoring driver. | source only |
| `R/15b_Claude_Full_Corpus.R` | Claude Sonnet 4.6 scoring | ~6 h |
| `R/16_Score_Full_Corpus_gpt41mini.R` | GPT-4.1-mini scoring | ~3 h |
| `R/18_Score_Full_Corpus_Gemini_flash_latest.R` | Gemini Flash scoring (with resume) | ~3 h |
| `R/19_Materialize_Gemini_Partial.R` | Helper if Gemini gets interrupted mid-run | as needed |
| `R/12_Validate_Span_Quality.R` | Span validation + LLM-as-judge backstop | sourced by drivers |
| `R/20_Spec_Curve_Compute.R` | Compute the 8,100-cell specification-curve grid | ~5-10 min |
| `R/21_Plot_Spec_Curve.R` | Generate `spec_curve.pdf` for the paper + per-fork CSV | <1 min |
| `R/4_Stats.R` | Statistical analysis, regression tables for the paper | 5 min |

All scoring drivers (`R/15b`, `R/16`, `R/18`) write per-batch checkpoints to
`data/CMP/v4_*_checkpoints/`. If a run is interrupted, just re-launch the
same driver — it will pick up where it left off.

---

## Reproducing the headline result

If you do not have the LLM API credits to re-score the corpus, you can skip
straight to the analysis using the published HuggingFace dataset:

```r
library(arrow)
sonnet <- read_parquet(
  "https://huggingface.co/datasets/sstoeckl/populism-llm/resolve/main/data/raw_sonnet.parquet"
)
gpt41  <- read_parquet(
  "https://huggingface.co/datasets/sstoeckl/populism-llm/resolve/main/data/raw_gpt41mini.parquet"
)
gemini <- read_parquet(
  "https://huggingface.co/datasets/sstoeckl/populism-llm/resolve/main/data/raw_gemini.parquet"
)
```

Place these three parquets in `data/CMP/poplib_v4_*_raw.parquet` (with the
right filename — see `R/18` header for the exact path), then run:

```r
source("R/20_Spec_Curve_Compute.R")   # produces website/data/specs_grid.parquet
source("R/21_Plot_Spec_Curve.R")       # produces paper figures
source("R/4_Stats.R")                  # reproduces the headline regressions
```

---

## Schema

The strict JSON schema enforced for every chunk × dimension is in
`R/3c_Populism_v4_safe.R::poplib_v4_schema`. Each scored dimension returns:

```json
{
  "score": <integer 0-10 or null>,
  "evidence_status": "verified" | "mixed" | "no_evidence",
  "span": {
    "quote": "<3-8 word verbatim quote>",
    "context": "<surrounding ~3 sentences>"
  }
}
```

The 12 scored dimensions are listed in the [methodology page of the
companion site](https://sstoeckl.github.io/populism-llm/methodology.html).

---

## Repo structure

```
populism-llm-replication/
├── R/                                  # All scoring + analysis scripts
│   ├── 1_Functions.R                   # Utilities
│   ├── 1_batch.R                       # (Optional) batch API
│   ├── 2_Manifesto_Prep.R              # CMP loader
│   ├── 3c_Populism_v4_safe.R           # Schema + prompts
│   ├── 4_Stats.R                       # Regressions
│   ├── 12_Validate_Span_Quality.R      # Validation
│   ├── 15b_Claude_Full_Corpus.R        # Sonnet driver
│   ├── 16_Score_Full_Corpus_gpt41mini.R# GPT-4.1-mini driver
│   ├── 18_Score_Full_Corpus_Gemini_*.R # Gemini driver
│   ├── 19_Materialize_Gemini_Partial.R # Recovery helper
│   ├── 20_Spec_Curve_Compute.R         # Multiverse
│   └── 21_Plot_Spec_Curve.R            # Paper figure
├── .Renviron.example                   # API key template
├── DESCRIPTION                         # R package metadata (for renv)
├── LICENSE                             # MIT
├── CITATION.cff                        # Cite-this metadata
└── README.md                           # This file
```

---

## License & citation

Code under MIT (see `LICENSE`).
Data products (scores, derived metrics) under CC-BY 4.0 via the HuggingFace
dataset card.
Original CMP manifesto texts are © Manifesto Project / WZB — obtain via
<https://manifesto-project.wzb.eu/>.

Please cite:

```bibtex
@article{rode_stoeckl_2026_milei,
  author  = {Rode, Martin and St{\"o}ckl, Sebastian},
  title   = {The Milei question, or: How liberal are right-wing populist
             parties? A measurement approach using large language models},
  year    = {2026},
  journal = {Working paper}
}

@misc{rode_stoeckl_2026_populismllm,
  author    = {Rode, Martin and St{\"o}ckl, Sebastian},
  title     = {Populism-LLM v1: A Cross-Validated LLM Annotation of
               European Party Manifestos},
  year      = {2026},
  publisher = {Hugging Face Datasets},
  url       = {https://huggingface.co/datasets/sstoeckl/populism-llm}
}
```

## Contact

Martin Rode · [martinrode@unav.es](mailto:martinrode@unav.es) · [Scholar](https://scholar.google.com/citations?user=CbCCz_EAAAAJ&hl=en)
Sebastian Stöckl · [sebastian.stoeckl@uni.li](mailto:sebastian.stoeckl@uni.li) · [homepage](http://www.sebastianstoeckl.com)
