Purpose
-------
This file gives concise, actionable guidance for AI coding agents working on this personal `dotfiles` repository. Focus on what is discoverable in the tree: Nix flakes, Home Manager, system modules, and helper scripts in `scripts/`.

Quick summary
-------------
- This repo is a Nix flake for two NixOS configurations (`fw12` and `fw16`) and a Home Manager `home.nix` that configures a single user `jon`.
- Primary entrypoints: `flake.nix`, `configuration.nix`, `home.nix`, and `scripts/`.
- Typical developer workflow: edit dotfiles in this repo, then run `nixos-rebuild --flake .` (see top-level `README.md`) or `nix flake update` followed by `sudo nixos-rebuild switch --flake .`.

What to change and where
------------------------
- System-wide NixOS options and hardware-specific modules live in `configuration.nix` and `hardware-configuration-*.nix`.
- User-level settings (packages, programs, services, dotfiles) are in `home.nix` and imported by Home Manager inside `flake.nix`.
- UI and editor config are under `dotfiles/doom/` (Doom Emacs) and `scripts/qutebrowser-userscripts/` (qutebrowser helpers).

Key patterns and conventions
---------------------------
- Flake outputs: `nixosConfigurations.fw12` and `nixosConfigurations.fw16` — prefer editing `home.nix` for user config, `configuration.nix` for system config, and `flake.nix` only to update inputs or target systems.
- Paths are hard-coded in `home.nix` via `dots = "/home/jon/Agordoj/dotfiles"` — be careful when refactoring paths; search for `dots` before renaming.
- Many service binaries are referenced via `${pkgs.<pkg>}/bin/<tool>` in Nix expressions. To change a binary, update the package list in the appropriate `programs` or `services` block.
- Shell helpers: fish functions and abbreviations are defined in `home.nix` under `programs.fish`. Prefer adding short aliases there.

Developer workflows (commands)
------------------------------
- Rebuild system (from repo root):

```bash
cd dotfiles
nixos-rebuild switch --flake .
```

- Update flakes and rebuild:

```bash
cd dotfiles
nix flake update
sudo nixos-rebuild switch --flake .
```

- Edit user Home Manager config and apply (if using Home Manager standalone, otherwise rebuild system as above):

```bash
# (home-manager is managed inside flake; normally rebuild the whole flake)
nix run .#homeConfigurations.jon.activationPackage --command home-manager switch
```

Repository-specific integration points
-------------------------------------
- `nix-doom-emacs-unstraightened` is imported in the flake and referenced in `home.nix`. When changing Emacs/Doom setup, update `dotfiles/doom/` and the flake inputs if necessary.
- `scripts/` contains utility scripts used by system services and programs (e.g. `org-clock.hs`, `pywal-reload.sh`, `downloadBook.py`). If you change a script, check references in `home.nix` (`xdg.dataFile`, `home.file`, or `systemd.user` services).
- qutebrowser userscripts are exposed via `xdg.dataFile` and the `qutebrowser` configuration in `home.nix` — changes to those scripts are applied when the flake is rebuilt.

Tests, linting, and debugging
-----------------------------
- There are no automated tests in this repository. Test changes by rebuilding the flake and verifying the targeted service.
- For debugging Nix expressions, use `nix repl` and `nix eval --json .#nixosConfigurations.fw12.config` to inspect computed config values.

Searchable examples (use these when producing edits)
----------------------------------------------------
- Rebuild command: see top-level `README.md` and `dotfiles/flake.nix`.
- User config: `dotfiles/home.nix` (programs, packages, fish functions).
- System modules: `dotfiles/configuration.nix`, `dotfiles/hardware-configuration-fw12.nix`, `dotfiles/hardware-configuration-fw16.nix`.
- Scripts: `scripts/` (many are referenced from Nix config). Example: `scripts/org-clock.hs` used by `waybar` in `home.nix`.

When editing
------------
- Prefer minimal, atomic changes and include a short explanation in the commit message describing why the change belongs in `home.nix` vs `configuration.nix` vs `flake.nix`.
- When renaming or moving files referenced in Nix expressions, update all references (search for filename across the repo); `nix build` will fail if paths are wrong.

What Not To Do
--------------
- Do not assume the repo is a generic dotfiles template — many paths and settings are user-specific (hard-coded `/home/jon/...`). Ask before making global path changes.

If something is unclear
----------------------
- Ask about the intended target machine (`fw12` or `fw16`) and whether changes should be user-only (`home.nix`) or system-wide (`configuration.nix` / flake inputs).

Request feedback
----------------
Please review this file and tell me what missing details you'd like added (e.g., extra commands, preferred branches, or test steps).
