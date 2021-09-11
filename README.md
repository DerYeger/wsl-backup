<h1 align="center">wsl-backup</h1>

<p align="center">
  Helper script for creating backups of WSL distributions.
</p>

<p align="center">
  <a href="https://github.com/DerYeger/wsl-backup/actions/workflows/ci.yml">
    <img alt="CI" src="https://img.shields.io/github/workflow/status/DerYeger/wsl-backup/CI?label=ci&logo=github&color=#4DC71F">
  </a>
  <a href="https://github.com/DerYeger/wsl-backup/releases/latest">
    <img alt="GitHub release (latest by date)" src="https://img.shields.io/github/v/release/DerYeger/wsl-backup">
  </a>
  <a href="https://opensource.org/licenses/BSD-3-Clause">
    <img alt="BSD-3-Clause" src="https://img.shields.io/github/license/DerYeger/wsl-backup">
  </a>
</p>

## Installation

Compiled binaries can be found [here](https://github.com/DerYeger/wsl-backup/releases/latest).

## Usage

The script can be used from the directory of the `wsl-backup.exe` file with the following command, where `directory` is the target directory and `distro` is the name of an installed WSL distribution.

```bash
wsl-backup [directory [distribution]]
```

If `distribution` is omitted, all installed distributions will be exported.
If `directory` is omitted, users are prompted to enter a directory.

### Examples

```bash
wsl-backup D:\backup Ubuntu-20.04
```

```bash
wsl-backup \\nas\backups
```

### Scheduled backups

Backups can be scheduled using [Task Scheduler](https://www.windowscentral.com/how-create-automated-task-using-task-scheduler-windows-10).

## License

[BSD-3-Clause](./LICENSE) - Copyright &copy; Jan Müller
