# Development Hints

## Live Reloading

To start a process that will watch the source files and trigger a reload
whenever they are modified use the following command:

```bash
npm run watch
```

If you're using an editor that supports `purs ide` or running [`pscid`][pscid],
there's an option for getting near instant builds of the application while you work:

```bash
npm run watch-fast
```


This will start a watch process that uses
[Webpack](https://github.com/webpack/webpack) to rebundle the app whenever the
_output_ files are changed. Since `purs ide` rebuilds modules on save, this
means you can use this much faster bundle-only rebuild script.

:warning: `purs ide` only rebuilds one module at a time, so sometimes the bundle
will end up in an inconsistent state, resulting in runtime errors. This occurs
when a change is made in one module that breaks other modules that depend on it.
The solution is to run a full build when a change like this is made, as the
compiler will force you to resolve those errors.

