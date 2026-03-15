# Dotfiles

Personal dotfiles managed with [chezmoi](https://www.chezmoi.io/).

## What's included

- **Zsh** — shell config with Oh My Zsh and Powerlevel10k
- **Git** — aliases, GPG signing, difftool/mergetool setup
- **tmux** — vim keybindings, plugin manager, iTerm2 integration
- **lf** — terminal file manager config
- **lazygit** — custom commands (e.g. create PR from terminal)
- **VS Code** — settings and keybindings ([keyboard shortcuts reference](code_shortcuts.md))

## Install chezmoi and apply

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply <github-username>
```

Or if you already have chezmoi installed:

```sh
chezmoi init <github-username>
chezmoi apply
```

## Restore from a local clone

```sh
chezmoi init --source /path/to/this/repo
chezmoi diff    # preview changes
chezmoi apply   # apply to home directory
```

## Day-to-day usage

```sh
# Edit a managed file
chezmoi edit ~/.zshrc

# Pull latest changes and apply
chezmoi update

# See what would change
chezmoi diff

# Add a new file to chezmoi
chezmoi add ~/.some/config
```

## Managed files

| Source | Target |
|--------|--------|
| `dot_zshrc` | `~/.zshrc` |
| `dot_gitconfig` | `~/.gitconfig` |
| `dot_tmux.conf` | `~/.tmux.conf` |
| `dot_config/lf/lfrc` | `~/.config/lf/lfrc` |
| `dot_config/lazygit/config.yml` | `~/.config/lazygit/config.yml` |
| `private_Library/Application Support/Code/User/settings.json` | `~/Library/Application Support/Code/User/settings.json` |
| `private_Library/Application Support/Code/User/keybindings.json` | `~/Library/Application Support/Code/User/keybindings.json` |
