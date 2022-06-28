# rebar3_lcov

Rebar3 plugin for converting erlang .coverdata to lcov tracefile format.

## Building

To build the project, run the following command:

```bash
$ make compile
```

## Development environment

### Run in a docker container

You can run any of the tasks defined in the Makefile from inside of a docker container (defined in `Dockerfile.dev`) by prefixing the task name with `wc-`. To successfully build the dev container you need `Docker BuildKit` enabled. This can be accomplished by either installing [docker-buildx](https://docs.docker.com/buildx/working-with-buildx/) locally, or exporting the `DOCKER_BUILDKIT=1` environment variable.

#### Example

* This command will run the `compile` task in a docker container:
```bash
$ make wc-compile
```

## Usage

Add to your `project_plugins` as follows:

```erlang
{project_plugins, [
    <...>
    {rebar3_lcov, {git, "https://github.com/valitydev/rebar3_lcov.git", {tag, "v1"}}},
    <...>
}
```

Configure it in your `rebar.config` as follows (those are the defaults):

```erlang
{lcov, [
    % Path to output file
    {output_file, "_build/test/lcov/lcov.info"},
    % Names of files in _build/test/cover to use as sources of coverage data
    {coverdata_files, ["eunit.coverdata", "ct.coverdata"]}
]}.
```

Run the plugin as follows:

```bash
$ rebar3 lcov convert
```

[1]: http://erlang.org/doc/man/shell.html
