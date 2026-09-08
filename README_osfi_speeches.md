# OSFI speeches ingestion

This repository now includes an R script, `osfi_speeches_ingest.R`, for a recurring OSFI speech ingestion workflow aimed at RAG pipelines.

## What it does

- Scrapes the OSFI speeches listing page and all linked pagination pages.
- Preserves a speech catalog in CSV format.
- Downloads only speeches that are new to the catalog.
- Extracts the speaker into metadata, including Peter Routledge, Ben Gully, or any other named speaker detected from the listing/body.
- Writes each speech to a plain-text `.txt` file.
- Builds chunk-level vectors for downstream retrieval.
- Detects speeches that disappeared from the OSFI listing and sets `removed_noticed_date` in the catalog.

## Main outputs

- `data/osfi_speeches/speech_catalog.csv`
- `data/osfi_speeches/speech_chunks.csv`
- `data/osfi_speeches/vectors/speech_chunk_vectors.jsonl`
- `data/osfi_speeches/text/*.txt`
- `data/osfi_speeches/run_log.csv`

## Suggested vectorization strategies

### 1) Deterministic hashed vectors (implemented by default)

Use this when you need an offline, dependency-light fallback.

- Pros: no API key, deterministic, cheap, good enough for a bootstrap pipeline.
- Cons: noticeably worse semantic recall than true embedding models.

### 2) OpenAI embeddings (recommended for production RAG)

Suggested upgrade path:

- Replace the `hash_embedding()` call with a batched API call to an embedding endpoint.
- Store one embedding per chunk.
- Keep the same `speech_id`, `chunk_id`, `speaker`, and `speech_date` metadata so your vector store and relational catalog stay aligned.

### 3) Local embeddings via Ollama or sentence-transformers

Use this when you need privacy or want to avoid per-call API costs.

- Run a local embedding model and call it from R over HTTP.
- Keep the same chunking logic and output schema.

## Scheduling

### Cron

```cron
15 6 * * * /usr/bin/Rscript /path/to/osfi_speeches_ingest.R >> /var/log/osfi_speeches.log 2>&1
```

### n8n

- Trigger: Schedule node.
- Action: Execute Command node running `Rscript /path/to/osfi_speeches_ingest.R`.
- Optional: add a second node to push new chunks into a database or vector store.

## Recommended next step for a full RAG pipeline

After this script runs, load `speech_chunks.csv` and `speech_chunk_vectors.jsonl` into one of the following:

- PostgreSQL + pgvector
- Qdrant
- Weaviate
- Pinecone
- Milvus

Use `speech_id`, `chunk_id`, `speaker`, `speech_date`, `topics`, and `removed_noticed_date` as filterable metadata.
