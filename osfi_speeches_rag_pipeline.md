# OSFI speech RAG pipeline

## What the script does

`osfi_speeches_rag_pipeline.R` scans the OSFI speeches listing, downloads only speeches that are new locally, saves them as plain text and HTML, creates vector-ready chunks, and records speeches that disappear from the live index with an explicit `deletion_date`.

## Output layout

- `data/osfi_speeches/metadata/speech_table.csv`
  - canonical speech registry
  - contains `primary_speaker`, `speaker_names`, `known_speaker_match`, `status`, `first_seen_on`, `last_seen_on`, `deletion_date`, and `removed_detected_on`
- `data/osfi_speeches/text/*.txt`
  - cleaned speech text files for direct RAG ingestion
- `data/osfi_speeches/html/*.html`
  - raw HTML snapshots for re-processing if extraction rules evolve
- `data/osfi_speeches/vector/speech_chunks.csv`
  - vector-ready chunk table
- `data/osfi_speeches/vector/speech_chunks.jsonl`
  - easy hand-off file for vector DB loaders or n8n
- `data/osfi_speeches/vector/speech_chunks.parquet`
  - optional, written when the `arrow` package is installed

## Suggested deployment patterns

### Option 1: CRON + vector DB loader
1. Run the R script daily.
2. Load `speech_chunks.jsonl` or `speech_chunks.parquet` into pgvector, Qdrant, Pinecone, Milvus, or Weaviate.
3. Upsert by `chunk_id` and keep `speech_id` plus `url` as metadata.

### Option 2: n8n orchestration
1. Cron trigger in n8n.
2. Execute Command node running `Rscript osfi_speeches_rag_pipeline.R`.
3. Read `speech_chunks.jsonl`.
4. Generate embeddings in n8n if you prefer to keep embedding outside R.
5. Upsert into the vector database.

### Option 3: End-to-end from R
Set `OSFI_EMBED_PROVIDER=openai` and `OPENAI_API_KEY` to have the script write embedding vectors into `speech_chunks.csv/jsonl` directly.

## Example CRON entry

```cron
15 6 * * * cd /workspace/automatisation && /usr/bin/env Rscript osfi_speeches_rag_pipeline.R >> /var/log/osfi_speeches.log 2>&1
```

## Example n8n command

```bash
cd /workspace/automatisation && Rscript osfi_speeches_rag_pipeline.R --output-dir data/osfi_speeches
```
