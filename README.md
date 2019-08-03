# tmdb-dumper
External dependency: `sqlite3`.

```
tmdb-dumper: A tool to dump data from TheMovieDB

Usage: tmdb-dumper COMMAND FILE

Available options:
  -V,--Version             Show current version
  -h,--help                Show this help text

Available commands:
  update                   Update/Create the database FILE
  info                     Show metadata information for the database FILE
```

## Update
```
Usage: tmdb-dumper update [-v] [--no-terminal] [-f|--force] (-m | -a)
  Update/Create the database FILE

Available options:
  -v                       Verbose output, repeat to increase verbosity
  --no-terminal            Disable terminal support for output
  -f,--force               Force updating all records ignoring existing data
  -m                       Update movies in database FILE from TheMovieDB
  -a                       Update Everything in database FILE from TheMovieDB
  -h,--help                Show this help text
```
