# Gmail Newsletter Digest with Ollama (R version)

This script pulls newsletter-like emails from Gmail and asks your local Ollama model to build a digest.

## 1) Setup

Install R packages:

```r
install.packages(c("argparse", "gmailr", "httr2", "jsonlite", "stringr"))
```

Enable Gmail API in Google Cloud and download OAuth Desktop credentials as `credentials.json` in this folder.

## 2) Optional preference profile

Create `preferences.txt` to guide ranking, for example:

```text
Prioritize AI engineering, data, Python, product strategy, and practical tutorials.
Down-rank generic e-commerce promotions and celebrity/entertainment topics.
```

## 3) Run

```bash
Rscript gmail_newsletter_digest.R \
  --query 'category:promotions newer_than:7d' \
  --max-messages 40 \
  --ollama-model llama3.1:8b \
  --preferences-file preferences.txt \
  --output newsletter_digest.md
```

Useful Gmail queries:
- `label:newsletters newer_than:14d`
- `category:promotions -from:(amazon.com) newer_than:7d`
- `from:(substack.com OR beehiiv.com) newer_than:10d`

## 4) Matching to your preferences (recommended approach)

Use a lightweight feedback loop:
1. Keep a `ratings.csv` with `message_id,rating` where rating is 1-5.
2. Add those examples to the prompt each run so Ollama learns your taste.
3. Promote senders/topics with avg rating >=4 and suppress <=2.
4. Once enough data exists, train a tiny local classifier (logistic regression on subject + snippet embeddings) and combine with LLM score.

This gives progressively better filtering while staying local/private.
