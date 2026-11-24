---
title: Cursor
type: docs
url: "hub/scripting/tools/visual-studio-code"
---
## What is Cursor?

[Cursor](https://cursor.com/) is a code editor with AI assistance in the same window. It is based on Visual Studio Code, so syntax highlighting, extensions, workspaces, and Git all carry over. Cursor is the editor used for Lumi scripting here, because that AI help is much more economical.

- **Syntax Highlighting**: Syntax highlighting color-codes your code, making it easier to spot errors, understand structure, and read complex code.
- **Extensions**: Cursor runs Visual Studio Code extensions. Whether you need support for a specific language, theme, or tool, you can find an extension that fits.
- **Version Control Integration**: Git is built in, making it easy to track your code changes and manage branches, using services like [GitHub](https://github.com/) or [GitLab](https://gitlab.com/).
- **AI in the editor**: You can ask for a search, a refactor, or a small edit beside the file you already have open. The cost of that help is low enough to use it as part of ordinary scripting.

### Key Features for Beginners

- **Workspaces**: Cursor lets you organize multiple projects into "workspaces," which keep related files and folders together for easier navigation and project management.

- **Profiles**: With profiles, you can create different setups for various projects or tasks. You might use one profile for web development and another for Scheme programming, each with its own extensions, themes, and settings.

### Getting Started with Cursor

To start using Cursor:

- **Download and Install**: Go to the [Cursor website](https://cursor.com/) and download the Linux version.

- **Explore the Interface**: When you open Cursor, you'll notice several panels. On the left, you have the **Activity Bar**, where you can navigate between files, search, use Git, and install extensions.

- **Open a File or Folder**: Set up your workspace by adding the folder where your project is stored, and save the active configuration to a workspace file.

A workspace is just a saved folder view plus editor settings for your project.

### Extensions

- **Install Extensions**: Click on the **Extensions** icon in the Activity Bar to browse and install helpful tools for Scheme. The main one to install is **"vscode-scheme"** by Allen Huang. This extension provides syntax highlighting and basic support for Scheme, the language Lumi plug-ins are written in.

You can also customize the extension by editing the language file to include more Scheme-specific keywords or anything related to Lumi scripting.

If you maintain your own helper library, adding your project’s function names to the syntax highlighter makes navigation and refactoring faster.

### Conclusion

Use an editor that makes it easy to search, refactor, and run quick edits, with AI close at hand. Cursor plus a Scheme extension is the baseline used here.
