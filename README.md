# Airput

CLI tool for populating Airsequel with data.
Includes a crawler for metadata of GitHub repos.


## Usage

```txt
⬆️ Airput ⬆️

Usage: airput COMMAND

  CLI tool for populating Airsequel with data.

Available options:
  -h,--help                Show this help text

Available commands:
  upload                   Upload files to a database via the REST API. Expects
                           3 columns: `name`, `filetype`, and `content`.
  github-upload            Upload metadata for a single GitHub repo
  github-search            Search for GitHub repos and upload their metadata.

                           If several search queries are provided, they will be
                             executed one after the other.

                           WARNING: If a search returns more than 1000 repos,
                             the results will be truncated.

                           Good search options are:
                           - language:haskell
                           - stars:>=10
                           - stars:10..50
                           - created:2023-10
                           - archived:true
```


## TODOs

- Add column `is_private` and only crawl private repos if `--private` is passed
- Add subcommand to load list of repos from Airsequel and update them
- Move `bin-calculation.py` to Airsequel
- Store if account is a person or an organization
- Store all languages for a repo
- Repos created per week chart
- Add CLI flag to choose between `OverwriteRepo` and `AddRepo`


## Related

- [GrimoireLab] - Open source tools for software development analytics.
- [SEART GitHub Search Engine] - Platform to crawl, store, and present repos.

[GrimoireLab]: http://chaoss.github.io/grimoirelab/
[SEART GitHub Search Engine]: https://github.com/seart-group/ghs
