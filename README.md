# Description

UI for dungeon.studio

# Getting Started

The UI is available at <https://my.dungeon.studio>.

This project uses [purescript-halogen] with [purescript-run] for extensible effects.

## Locally with [`docker-compose`][docker-compose]

This project is setup to run with [`docker-compose`][docker-compose].  Running
the folliwng command will build a [docker] image, and start all requisite
services as [docker] containers.

```bash
docker-compose up -d
```

## Locally with [`npm`][npm]

This project utilizes [`npm`][npm] and local builds can use the following setup:

```bash
npm install --global bower # required for purescript
npm install
npm run build
```

# Reporting Issues

Any issues discovered should be recorded on [github][issues].  If you believe
you've found an error or have a suggestion for a new feature; please, ensure
that it is reported.

If you would like to contribute a fix or new feature; please, submit a pull
request.  This project follows [git flow] and utilizes [travis] to automatically
check pull requests before a manual review.

# Contributors

The `COPYRIGHT` file contains a list of contributors with their respective
copyrights and other information.  If you submit a pull request and would like
attribution; please, add yourself to the `COPYRIGHT` file.

[docker-compose]: https://docs.docker.com/compose/
[docker]: https://docs.docker.com/
[git flow]: http://nvie.com/posts/a-successful-git-branching-model/
[issues]: https://github.com/velveteer/my.dungeon.studio/issues
[npm]: https://www.npmjs.com/
[pscid]: https://github.com/kRITZCREEK/pscid
[purescript-halogen]: https://github.com/slamdata/purescript-halogen
[purescript-run]: https://github.com/natefaubion/purescript-run
[travis]: https://travis-ci.org/velveteer/my.dungeon.studio
