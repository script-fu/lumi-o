---
title: "Installing Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---

This document outlines the process used to install Debian Stable as the Lumi·o development operating system. It may be useful for others setting up a similar environment.

Debian Stable was selected because Lumi aims to build reliably on a predictable long-term platform. GIMP development targets Debian Testing, making Debian Stable a closely aligned base system.

If you are coming from Windows, the main conceptual change is that most software installation and configuration happens through package managers and simple terminal commands rather than downloadable installers.

## Who This Guide Is For

This guide documents a working Debian Stable setup used for Lumi development. It is not a general Linux installation tutorial.

It is most useful for:

- artists moving from Windows who want a predictable Linux setup
- developers building Lumi from source
- users who prefer reproducing a known working environment rather than designing their own system configuration

Basic familiarity with disk partitioning and simple command-line usage is assumed.

## Backup Your Data

Before installing Debian, create a complete backup of your Home directory on an external drive. Include any additional data folders you want to preserve.

Note: In Linux, `~` represents your Home directory.

If you use Git repositories, push any important changes to their origins so they can be restored easily after installation. This step is only relevant if you already use Git.

## Create a Partition

Create space on your primary drive for Debian. Many guides and tools exist for this step, including GParted. Depending on your setup, you may:

- shrink an existing Windows partition for dual boot
- reuse an existing Linux partition
- prepare new Linux and swap partitions

If you are unsure, consult hardware-specific guides before making changes, as partitioning steps vary significantly between systems.


## Create a Debian Installation USB

Assuming a target partition and swap space already exist:

1. Download the Debian ISO from the official website: https://www.debian.org/
2. On Windows, use BalenaEtcher to write the ISO to a USB drive.
3. On Linux, use a command-line tool such as `dd` to create a bootable USB.

## Install Debian

1. Insert the USB drive.
2. Restart and press the boot menu key (commonly `F2`, `F12`, `Esc`, or `Del`) during startup.
3. Select the USB device.
4. Choose a non-graphical installer.
5. Leave the root password blank when prompted so the installer grants sudo access to your user account.
6. Partition manually:

   - Filesystem: ext4 (journaling)
   - Swap: existing swap partition
   - Mount point: `/`
   - Label: `linux`
   - Hostname: system name shown as `user@hostname`
   - User account: your full name
   - Username: terminal login name

7. Select **Cinnamon** as the desktop environment.
8. Complete installation and reboot into Debian Stable.

## System Setup

### Display Scaling

Debian Stable currently handles fractional scaling inconsistently, especially on 4K displays. Instead of reducing display resolution, adjust interface elements directly.

Recommended adjustments:

- Avoid fractional display scaling.
- Menu → Font Selection → Font Settings → Text Scaling Factor: `2.5`
- Desktop Font: `14`
- Panel → Customize → Panel Height: `60`
- Panel Appearance → Right Zone Symbolic Icon Size: `48px`
- Mouse and Touchpad → Pointer Size adjustment
- Desktop (right-click) → Customize → Larger icon size

Firefox adjustment:

- Address bar → `about:config`
- Set `layout.css.devPixelsPerPx` to `1`

### Terminal

Configure terminal preferences:

1. Menu → Terminal → Edit → Preferences
2. Text → Initial size: `140 columns`, `40 rows`
3. Text → Custom font: `Monospace 10`
4. Colours → Built-in schemes → Solarized Dark

## Restore Data

Restore backed-up files into the Home directory as needed, for example:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Note: Folders beginning with `.` are hidden configuration directories in Linux.

## Optional: Git Setup

Required only if you plan to build Lumi or restore repositories.

### Install Git

```bash
sudo apt install git
```

Configure your identity:

```bash
git config --global --edit
```

#### GitLab Access

Restore repository access to GitLab or GitHub:

1. Change the permissions on the SSH key file: `chmod 600 ~/.ssh/id_rsa`
2. Add the user to the new Git install: `ssh-add ~/.ssh/id_rsa`
3. Test the connection: `ssh -T git@ssh.gitlab.gnome.org` or `ssh -T git@github.com`

For each repository, fetch the origins and reset the local branch to match:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Run `git status` to confirm repositories are clean.

We now have a new OS with any data and repositories restored. This setup reflects a known working environment used for Lumi development and can be adapted to individual workflows as needed.

## Build Lumi After OS Setup

Lumi build scripts are located in:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```


