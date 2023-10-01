# Llama-cpp in Emacs

*The readme is mostly written by CodeLLama.* :satisfied:

Llama package for Emacs provides a client for the
[llama-cpp](https://github.com/ggerganov/llama.cpp) server. It allows you to ask
llama for code completion and perform tasks within specified regions of the
buffer.

## Table of Contents

1. [Installation](#installation)
2. [Usage](#usage)
3. [Customization](#customization)
4. [License](#license)

## Installation

To install this package, add it to your Emacs configuration:

```elisp
(use-package llama-cpp
  :ensure t)
```

Make sure to have [llama-cpp](https://github.com/ggerganov/llama.cpp) server
running and accessible from the host specified in `llama-cpp-host` variable (default
is "localhost"). The server should be accessible via HTTP on the port specified
in `llama-cpp-port` (default is 8080).

## Usage

This package provides `llama-cpp-complete` as a public API for other packages to
use.

To cancel a running llama process, use `M-x llama-cpp-cancel`.

### Chat

You can start a Llama chat session using the command `M-x llama-cpp-chat-start`.

After providing the necessary input, you can execute `M-x llama-cpp-chat-answer`
to request a response from Llama.

To complete text from the llama buffer, use `M-x llama-cpp-chat-complete`.

### Code

`M-x llama-cpp-code-region-task` asks for a description of your task and then calls
the llama to provide an answer. The task context is taken from the region you select
when calling this function.

## Customization

The following customization options are available:

* `llama-cpp-host`: the host of the llama-cpp server (default is "localhost").
* `llama-cpp-port`: the port of the llama-cpp server (default is 8080).
* `llama-cpp-chat-prompt`: sets the chat prompt to start the chat with.
* `llama-cpp-chat-input-prefix`: string to prefix user inputs with.
* `llama-cpp-chat-input-suffix`: string to suffix after user inputs with.
* `llama-cpp-code-lang-modes`: an alist mapping major modes to their language names.
* `llama-cpp-code-region-prompt`: a prompt template for code region tasks.

## License

This package is distributed under the GNU General Public License version 3 or
later. Please refer to the
[official repository](https://github.com/kurnevsky/llama-cpp.el) for more information.
