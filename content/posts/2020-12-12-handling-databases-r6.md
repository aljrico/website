---
layout: post
title: "Handling multiple database connections with R"
date: 2020-12-12
draft: false
toc: false
images: 
Cover:
tags:
  - r
  - programming
---

From its origin, R has been conceived as a programming language aimed to excel at statistic analysis, and as a mathematical tool. Technically capable of performing almost anything you can think of, as it would be expected from a general programming language such as Python. But both the specific functionality of the language, and the whole community, culture and universe of libraries that has been built upon R, has made of it a niche programming tool, used to address a relatively narrow set of tasks.

Though this has been changing in recent years. As the rise of Data Science has bubbled up a new interest in statistics, the usage of R has skyrocketted. This has encouraged developers to design more and more R libraries, paving the way to start using R as a general purpose programming language.

One of the areas in which R has excelled above other languages, is in the availability and capability of data visualisation libraries. For many developers and analysts, R is the go-to language when it comes to processing, cleaning and displaying data in a beautiful and understandable way. It's easy-to-use, it's powerful, and its community grows bigger and wider every day. It was only a matter of time than this evolution would lead to the use-case that is web app development.

By the hand of the 'shiny' library, web app development with R has become widely popular in many industries. It has helped to bring together the best of R original toolkit and known libraries, and to push it in the direction of becoming a more general language. 

Although originally used to build simple dashboards, shiny development has grown to manage huge and complicated applications used for many purposes in academica, corporations or as stand-alone apps for the public. Even indie videogames have been created with it. Despite this growth, there's not much information out there about proper software development with R or shiny, particularly when it comes to handling connection and access to the databases. This article here aims to summarise some of those basics for R developers.

Dealing with credentials
--------------------

So we are getting started with the most delicate bit of managing access to the databases. Credentials. Please remember that I'm only displaying the very basics upon which you can start code your own solution to manage credentials. Out there are plenty of built and tested solutions that could be more reliable if you are handling delicate information. But we'll start with the most basic way of handling this: Hashing.

## Hashing

So let's say that a user wishes to register and create a user with our app, we have a nice UI that captures `username` and `password`. The UI is so fancy that hides the password as the user enters it, but of course the server can read that. So we get the following information:

```{r}
username = input$signup_username
password = input$signup_password
```

We need to store this information somewhere so we're capable of checking the user's credentials every time they want to log in. But we are not supposed to actually know that password, even less to store it like that. So we need to encrypt that password. A cool library that I like to do just that is `digest`. So to make sure we only save a hashed password, our server code should look more like this:

```{r}
username = input$signup_username
hash = digest::digest(input$signup_password, algo = "md5")
```

This library lets you pick whatever algorithm you want to encrypt objects. As long as you always use the same one, any should work fine. So now we can store this credentials, for example, in a data.table:

```{r}
credentials_dt = data.table::data.table(usernames = username, passwords = hash)
```

This data.table should be stored in a safe database and keep binding new users as they sign up. So now that we have stored a table with all registered users and their credentials in a safe way, we can let them log in. Now let's say that we have another pretty piece of UI that captures users log in, we get

```{r}
login_username = input$login_username
login_hash = digest::digest(input$login_password, algo = "md5")
```

Note that we are encrypting the login password with the exact same algorithm as we did with the sign up. Since this is a one-way algorithm, the result is deterministic. Which means that if the user has introduced the correct password, the encrypted login password will be the same as the signup pasword. So checking user credentials ends up being as easy as just filtering that table:

```{r}
user_credentials = credentials_dt[usernames == login_username & passwords == login_hash]
if(nrow(user_credentials) > 0 ) print("Success!") else print("Wrong username or password")
```

So this way we have managed to capture user credentials, store them and then check that the login is correct. All of that without actually knowing or storing their actual password anywhere. You could argue that this encrypting method is kind of shallow. And it is. Let's say that a malicious agent gets access to the database where we have all the credentials stored. The passwords are encrypted, so they can't be seen. But the way they're encrypted still presents a vulnerability: Two equal passwords will yield the exact same hash. Brute force is not a realistic threat though, as these hashing algorithms are - by design - quite slow.

Handling Database Connections
--------------------

Now that we have a rough idea of how to manage and store credentials, we can start talking about how to use them to connect our shiny app to a database. So let's say that a user has logged in and we haved checked he's properly registered. At this point ideally we'd also have given that user certain permissions in the database we want them to have access to. So we can simply open a connection using the `odbc` library:

```{r}
connection_string = paste0(
  "Driver=", "{ODBC Driver 42 for SQL Server}",
  ";Server=", "sql-abc-rstudio-dev,1234",
  ";Database=", "ProjectDataBase",
  ";UID=", login_username,
  ";PWD=", login_hash
)

connection = odbc::dbConnect(odbc::odbc(), .connection_string = connection_string, encoding = "UTF-8")
```

It's likely the user will need to query many things in a session, so we can just keep this connection open and send queries that way:

```{r}
sql_string = "SELECT * FROM main_table"
odbc::dbGetQuery(connection, DBI::SQL(sql_string))
```

Eventually we'll need to close that connection, which is as easy as:

```{r}
odbc::dbDisconnect(connection)
```

Deciding when to close the connection can actually get quite tricky. Typically, you'd do it whenever the user logs out. But what if the user doesn't log out and just closes the app? Or goes inactive and the app freezes? Or maybe some error pops up and stops everything. And that gets far more complicated if we want to allow multiple connections on the same user. The immediate reaction to this, is to pepper all the server code with closing statements. `close_connection()` when the session ends, when the user logs out, when inactivity is detected, after any detectable error, etc. In my experience, going down this route tends to flood the app with overly complicated code, very prone to errors and difficult to maintain.

My proposal here is simply to open conenctions only when they are needed, and close them immediately after using them. The main caveat of this is that you are creating a huge overhead. Especially if we know a user is going to request access to the database many times while casually browsing the app. 

Then, how should we implement this?

### R6

Not many people are aware that the typical Object Oriented Programming everyone knows is also possible with R. It's not as popular as S3 or so, but it's possible with the `R6` library. Without going into full detail on how to use R6, I'll just go through a very basic connections handler.

The main advantage of constructing an R6 class to handle connections, is that we can very tidy when designing what it needs. Let's start with the fields it'll need:

```{r}
ConnectionHandler = R6::R6Class(
  "ConnectionHandler",
  private = list(
    username = character(0),
    password = character(0),
    connection_object = NULL
  )
)
```

So we have created a list of values that we know will need to be filled. And we also know that not even the developer should ever need to access them directly. So they're all private fields. Now we need to define a method that actually fills them with user input.

```{r}
ConnectionHandler = R6::R6Class(
  "ConnectionHandler",
  private = list(
    username = character(0),
    password = character(0),
    connection_object = NULL
  ),
  
  public = list(
    capture_credentials = function(username, password){
      private$username = username
      private$password = digest::digest(password, algo = "md5")
    }
  )
)
```

So now we have a class that will be capable of creating an instance object, capture a user credentials and store them as their field. And it would be used like this:

```{r}
connection_handler = ConnectionHandler$new()
connection_handler$capture_credentials(username = input$login_username, password = input$login_password)
``` 

So now we have the basics, let's go to actually handling database connections. We'll start by adding another field with the whole connection string, and then defining private methods that open and close those connections:

```{r}
ConnectionHandler = R6::R6Class(
  "ConnectionHandler",
  private = list(
    username = character(0),
    password = character(0),
    connection_object = NULL,
    connection_string = character(0),
    open_connection = function() {
      private$connection_object = odbc::dbConnect(odbc::odbc(), .connection_string = self$connection_string, encoding = "UTF-8")
    },
    close_connection = function() {
      odbc::dbDisconnect(private$connection_object)
    },
  ),
  
  public = list(
    capture_credentials = function(username, password){
      private$username = username
      private$password = digest::digest(password, algo = "md5")
      private$connection_string = paste0(
        "Driver=", "{ODBC Driver 42 for SQL Server}",
        ";Server=", "sql-abc-rstudio-dev,1234",
        ";Database=", "ProjectDataBase",
        ";UID=", private$username,
        ";PWD=", private$password
      )
    }
  )
)
```

Note that we've essentially pasted the `connection_string` we created before into the `capture_credentials` method. This way the moment credentials are introduced and verified we have the database access details ready to go.

So to make this complete, we only need to add the remaining piece: The method that allows to user to actually query stuff.

```{r}
ConnectionHandler = R6::R6Class(
  "ConnectionHandler",
  private = list(
    username = character(0),
    password = character(0),
    connection_object = NULL,
    connection_string = character(0),
    open_connection = function() {
      private$connection_object = odbc::dbConnect(odbc::odbc(), .connection_string = self$connection_string, encoding = "UTF-8")
    },
    close_connection = function() {
      odbc::dbDisconnect(private$connection_object)
    }
  ),
  
  public = list(
    capture_credentials = function(username, password){
      private$username = username
      private$password = digest::digest(password, algo = "md5")
      private$connection_string = paste0(
        "Driver=", "{ODBC Driver 42 for SQL Server}",
        ";Server=", "sql-abc-rstudio-dev,1234",
        ";Database=", "ProjectDataBase",
        ";UID=", private$username,
        ";PWD=", private$password
      )
    },
    query = function(sql_string){
      private$open_connection()
      odbc::dbGetQuery(private$connection_object, DBI::SQL(sql_string))
      private$close_connection()
    }
  )
)
```


### How would that be used

Essentially, this is it. We've got an R6 class that can capture and store credentials and uses them to handle access to a database. And this is how it'd be used inside a shiny app:

```{r}
connection_handler = ConnectionHandler$new()
connection_handler$capture_credentials(username = input$login_username, password = input$login_password)

sql_string = "SELECT * FROM main_table"
connection_handler$query(sql_string)
```

