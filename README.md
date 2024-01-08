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

- [ ] Add a column repo.exclusion_reason
      If `!= NULL` repo should be excluded from further processing
      E.g. for forks, mirrors, etc.
- [ ] Add subcommand to load list of repos from Airsequel and update them
- [ ] Add CLI flag to choose between `OverwriteRepo` and `AddRepo`
- [ ] Store all languages for a repo
- [ ] Store if account is a person or an organization


## Related

- [GrimoireLab] - Open source tools for software development analytics.
- [SEART GitHub Search Engine] - Platform to crawl, store, and present repos.

[GrimoireLab]: http://chaoss.github.io/grimoirelab/
[SEART GitHub Search Engine]: https://github.com/seart-group/ghs
