# deskjet

**deskjet** is an interactive HTTP utility written in Erlang to make API testing and HTTPS calls easier. It supports TLS certificates, JSON file requests, and repeated synchronous or asynchronous execution.

---

## ✨ Features

- Supports HTTP methods: `GET`, `POST`, `PATCH`, `PUT`, `DELETE`
- Send files as JSON request bodies
- Loop execution with `loop` (synchronous) and `loop_async` (concurrent)
- Works with secure HTTPS connections (SSL/TLS via `inets` and `ssl`)
- Returns are normalized as `{ok, Body}` or `{error, Reason}`

---

## 📦 Installation

1. Clone or copy the `deskjet_http.erl` file into your project directory.
2. Compile using the Erlang shell:

   ```erlang
   1> c(deskjet_http).
   {ok, deskjet_http}
   ```

3. Start required libraries:

   ```erlang
   2> deskjet_http:start().
   ok
   ```

---

## 🔐 SSL Certificate

This library requires a `.pem` file for HTTPS validation.

You can get the `cacert.pem` file from:  
👉 https://curl.se/docs/caextract.html

Save the file as `../cert/cert.pem` (or adjust the path in the code).

---

## 🧪 Usage Examples

### GET Request

```erlang
deskjet_http:get("https://example.com").
```

### POST with JSON

```erlang
deskjet_http:post("https://api.com/endpoint", "{"key":"value"}").
```

### PATCH with file

```erlang
deskjet_http:patch_with_file("https://api.com/update", "body.json").
```

### Using `perform/3`

```erlang
deskjet_http:perform("https://example.com", "{"key":"value"}", "post").
```

### Using file-based request

```erlang
deskjet_http:perform_with_file("https://example.com", "body.json", "put").
```

---

## 🔁 Loop Execution

### Synchronous loop

```erlang
deskjet_http:loop(10, fun() -> io:format("Test~n") end).
```

### Asynchronous loop

```erlang
deskjet_http:loop_async(10, fun() -> io:format("Test Async~n") end).
```

---

## 🧱 Module Overview

- `start/0`: Initializes `inets` and `ssl`
- `get/1`, `post/2`, `patch/2`, `put/2`, `delete/1`: Basic HTTP methods
- `*_with_file/2`: Reads file and sends its content as request body
- `perform/1..3`: Simplified method interface
- `loop/2`, `loop_async/2`: Repeats a function multiple times (sync or async)

---

## 🧑‍💻 Author

- **Teichmuller** – [matheussilvatech@gmail.com](mailto:matheussilvatech@gmail.com)