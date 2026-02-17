---
title: Hello World!
type: docs
weight: 1
---

This tutorial walks through the minimal structure of a Scheme plug-in. Some lines are “boilerplate”: they are required for Lumi to load the file, even if you don’t fully understand them yet.

```bash
#!/usr/bin/env lumi-scheme-interpreter-0.1
```

At a high level you will:

1. Define a function
2. Register it so it appears in the Procedure Database
3. (Optional) Add a menu entry
4. Install the file in a plug-ins folder

### Define a Function

A function, also known as a _procedure_, is a chunk of code with a name and purpose, it takes an input and produces output.

**Input** > **_Function_** > **Output**

### Register the Function

Registering is the act of putting the function name on a list so that Lumi knows about it.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Link to the Menu

This tells Lumi where to find your function in its menu system.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

This displays the menu "Funky" in the main menu bar. Change the path to put the plug-in somewhere else. The path `<Image>/Funky` means the plug-in will appear under the **Image** menu category. You can change `<Image>` to `<Tools>`, `<Filters>`, etc., depending on where you want the plug-in to appear.

### Comments

In Scheme, Scheme's base language, comments are generally done by preceding a helpful line of text with `;;`. Your use of comments will depend on your fluency as a coder—if you code occasionally, more comments will help. If you code all the time, the code is as easy to read as the comment would be. Also, when programming functionally, the code tends to become descriptive enough to read like a script.

### Syntax

Code tends to have little rules around how to place items in a line, so that we can read the line easily. For example, a sentence may have a space after a comma or period. It helps the readability.

Code may arrange things in a similar manner, which may look odd at first:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Example Code

Here is the complete example. Most Lumi procedures are prefixed with `lumi-`. For example, `lumi-message` prints a string to the configured message handler.

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### Install the Plug-in

1. Go to **Lumi -> Edit -> Preferences -> Folders -> Plug-ins**.
2. Add your [repo](/hub/scripting/tools/git) plug-ins folder to the list.
3. Create a folder for the plug-in and save the example code above as `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Right-click on the `hello-world.scm` file.
5. Go to **Properties -> Permissions -> Allow executing file as program**.
6. Restart Lumi.

### Try the Plug-in

The plug-in should now appear under the "Funky" menu in the main Lumi window. Click it, and it should display the "Hello world!" message. Try modifying the code, like changing the message text, and save the file. When you run the plug-in again, your changes will be reflected without restarting Lumi.

Try experimenting by changing the menu path. For example, `"<Image>/File"` will put it inside the File menu, and `"<Image>/File/Funky"` will create a new section in the File menu. This is a great way to customize where your plug-in appears and to organize your tools.
