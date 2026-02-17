---
title: The Procedure Browser
type: docs
weight: 1
url: "hub/scripting/tutorials/first-step/the-procedure-browser"
---

The **Lumi Procedure Browser** lets you search the available procedures (built-in and plug-in provided) and inspect their parameters and return values.

### Where to Find the Lumi Procedure Browser

You can access the Procedure Browser in Lumi through the **Help** menu:

- **Help** -> **Procedure Browser**

### What the Procedure Browser Does

The Procedure Browser lists all of Lumi's internal procedures, along with those added by plug-ins, including the one you've just installed. Each procedure entry provides useful information, including:

- The procedure name.
- A description of what it does.
- The parameters it accepts (input values).
- The return values (output).

Search by keyword or procedure name when you need to verify a call signature or confirm the exact procedure name.

#### (lumi-message) in the Procedure Browser

Search for `lumi-message` to see its parameters and return values.

### Finding Your Plug-in

Once you've installed the "Hello World!" plug-in, you can find it listed in the Procedure Browser. Simply search for the function name that you registered with Lumi, in this case, "scheme-hello-world". The entry will display the parameters and any return values associated with the plug-in, along with a brief description. You'll also see where some of the text lines you entered as input parameters during the registration process are displayed under the **Additional Information** section.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

This makes it easy to verify that your plug-in is properly registered and gives you a quick way to review how it interacts with other procedures in Lumi. The Procedure Browser is a powerful tool for debugging and expanding your plug-ins by exploring all available procedures within Lumi.
