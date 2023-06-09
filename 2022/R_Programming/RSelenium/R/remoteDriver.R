#' CLASS remoteDriver
#'
#' remoteDriver Class uses the JsonWireProtocol to communicate with the
#'    Selenium Server. If an error occurs while executing the command then
#'    the server sends back an HTTP error code with a JSON encoded response
#'    that indicates the precise Response Error Code. The remoteDriver
#'    class inherits from the \code{errorHandler} class. If no error
#'    occurred, then the subroutine called will return the value sent back
#'    from the server (if a return value was sent).
#'    So a rule of thumb while invoking methods on the driver is if the
#'    method did not return a status greater then zero when called, then
#'    you can safely assume the command was successful even if nothing was
#'    returned by the method.
#'
#' remoteDriver is a generator object. To define a new remoteDriver class
#'    method `new` is called. The slots (default value) that are user
#'    defined are:
#'      remoteServerAddr(localhost), port(4444), browserName(firefox),
#'      version(""), platform(ANY),
#'      javascript(TRUE). See examples for more information on use.
#'
#' @importFrom caTools base64decode
#' @importFrom httr parse_url build_url
#' @field remoteServerAddr Object of class \code{"character"}, giving the
#'    ip of the remote server. Defaults to localhost
#' @field port Object of class \code{"numeric"}, the port of the remote
#'    server on which to connect
#' @field browserName Object of class \code{"character"}. The name of the
#'    browser being used; should be one of {chrome|firefox|htmlunit|
#'    internet explorer|iphone}.
#' @field path base URL path prefix for commands on the remote server.
#'    Defaults to "/wd/hub"
#' @field version Object of class \code{"character"}. The browser version,
#'    or the empty string if unknown.
#' @field platform Object of class \code{"character"}. A key specifying
#'    which platform the browser is running on. This value should be one
#'    of {WINDOWS|XP|VISTA|MAC|LINUX|UNIX}. When requesting a new session,
#'    the client may specify ANY to indicate any available platform may be
#'    used.
#' @field javascript Object of class \code{"logical"}. Whether the session
#'    supports executing user supplied JavaScript in the context of the
#'    current page.
#' @field nativeEvents Object of class \code{"logical"}. Whether the
#'    session supports native events. n WebDriver advanced user
#'    interactions are provided by either simulating the Javascript events
#'    directly (i.e. synthetic events) or by letting the browser generate
#'    the Javascript events (i.e. native events). Native events simulate
#'    the user interactions better.
#' @field serverURL Object of class \code{"character"}. Url of the remote
#'    server which JSON requests are sent to.
#' @field sessionInfo Object of class \code{"list"}. A list containing
#'    information on sessions.
#' @include errorHandler.R
#' @export remoteDriver
#' @exportClass remoteDriver
#' @aliases remoteDriver
#' @examples
#' \dontrun{
#' # use default server initialisation values
#' remDr <- remoteDriver$new()
#'
#' # send request to server to initialise session
#' remDr$open()
#'
#' # navigate to R home page
#' remDr$navigate("http://www.r-project.org")
#'
#' # navigate to www.bbc.co.uk notice the need for http://
#' remDr$navigate("http://www.bbc.co.uk")
#'
#' # go backwards and forwards
#' remDr$goBack()
#'
#' remDr$goForward()
#'
#' remDr$goBack()
#'
#' # Examine the page source
#' frontPage <- remDr$getPageSource()
#'
#' # The R homepage contains frames
#' webElem <- remDr$findElements(value = "//frame")
#' sapply(webElem, function(x) {
#'   x$getElementAttribute("name")
#' })
#'
#' # The homepage contains 3 frames: logo, contents and banner
#' # switch to the `contents` frame
#' webElem <- remDr$findElement(using = "name", value = "contents")
#' remDr$switchToFrame(webElem$elementId)
#'
#' # re-examine the page source
#'
#' contentPage <- remDr$getPageSource()
#' identical(contentPage, frontPage) # false we hope!!
#'
#' # Find the link for the search page on R homepage. Use xpath as default.
#' webElem <- remDr$findElement(value = '//a[@@href = "search.html"]')
#' webElem$getElementAttribute("href")
#' # http://www.r-project.org/search.html
#'
#' # click the search link
#' webElem$clickElement()
#'
#' # FILL OUT A GOOGLE SEARCH FORM
#' remDr$navigate("http://www.google.com")
#'
#' # show different methods of accessing DOM components
#'
#' webElem1 <- remDr$findElement(using = "name", value = "q")
#' webElem2 <- remDr$findElement(
#'   using = "id",
#'   value = webElem1$getElementAttribute("id")[[1]]
#' )
#' webElem3 <- remDr$findElement(
#'   using = "xpath",
#'   value = '//input[@@name = "q"]'
#' )
#'
#' # Enter some text in the search box
#'
#' webElem1$sendKeysToElement(list("RSelenium was here"))
#'
#' # clear the text previously entered
#'
#' webElem1$clearElement()
#'
#' # show an example of sending a key press
#' webElem1$sendKeysToElement(list("R", key = "enter"))
#'
#' # Collate the results for the `R` search
#' googLinkText <- remDr$findElements(value = "//h3[@@class = 'r']")
#' linkHeading <- sapply(googLinkText, function(x) x$getElementText())
#' googLinkDesc <- remDr$findElements(value = "//div[@@class = 's']")
#' linkDescription <- sapply(googLinkDesc, function(x) x$getElementText())
#' googLinkHref <- remDr$findElements(value = "//h3[@@class = 'r']/a")
#' linkHref <- sapply(
#'   googLinkHref,
#'   function(x) x$getElementAttribute("href")
#' )
#'
#' data.frame(
#'   heading = linkHeading,
#'   description = linkDescription, href = linkHref
#' )
#'
#' # Example of javascript call
#' remDr$executeScript("return arguments[0] + arguments[1];", args = 1:2)
#' # Example of javascript async call
#' jsscript <-
#'   "arguments[arguments.length - 1](arguments[0] + arguments[1]);"
#' remDr$executeAsyncScript(jsscript, args = 1:2)
#'
#' # EXAMPLE INJECTING INTO PHANTOMJS using phantomExecute
#' require(RSelenium)
#' pJS <- wdman::phantomjs(port = 4932L)
#' remDr <- remoteDriver(browserName = "phantomjs", port = 4932L)
#' remDr$open(silent = TRUE)
#' remDr$navigate("http://ariya.github.com/js/random/")
#' # returns a set of random numbers
#' remDr$findElement("id", "numbers")$getElementText()[[1]]
#' #  # now try injecting a new Math,random function
#' result <- remDr$phantomExecute("var page = this;
#'                                page.onInitialized = function () {
#'                                page.evaluate(function () {
#'                                Math.random = function() {return 42/100}
#'                                })
#'                                }", list())
#' remDr$navigate("http://ariya.github.com/js/random/")
#' # Math.random returns our custom function
#' remDr$findElement("id", "numbers")$getElementText()[[1]]
#' remDr$close()
#' pJS$stop()
#' }
#'
remoteDriver <-
  setRefClass(
    Class = "remoteDriver",
    fields = list(
      remoteServerAddr = "character",
      port = "numeric",
      browserName = "character",
      path = "character",
      version = "character",
      platform = "character",
      javascript = "logical",
      nativeEvents = "logical",
      extraCapabilities = "list",
      serverURL = "character",
      sessionInfo = "list"
    ),
    contains = "errorHandler",
    methods = list(
      initialize =
        function(
                 remoteServerAddr = "localhost",
                 port = 4444,
                 browserName = "firefox",
                 path = "/wd/hub",
                 version = "",
                 platform = "ANY",
                 javascript = TRUE,
                 nativeEvents = TRUE,
                 extraCapabilities = list(),
                 ...) {
          remoteServerAddr <<- remoteServerAddr
          port <<- port
          browserName <<- browserName
          path <<- path
          version <<- version
          platform <<- platform
          javascript <<- javascript
          nativeEvents <<- nativeEvents
          extraCapabilities <<- extraCapabilities
          serverURL <<- paste0("http://", remoteServerAddr, ":", port, path)
          callSuper(...)
        },
      show = function() {
        print(
          list(
            remoteServerAddr = remoteServerAddr,
            port = port,
            browserName = browserName,
            version = version,
            platform = platform,
            javascript = javascript,
            nativeEvents = nativeEvents,
            extraCapabilities = extraCapabilities
          )
        )
      },
      showErrorClass = function() {
        print(
          list(
            status = status,
            statusclass = statusclass,
            sessionid = sessionid,
            hcode = hcode,
            value = value
          )
        )
      },
      open = function(silent = FALSE) {
        "Send a request to the remote server to instantiate the browser."
        if (!silent) print("Connecting to remote server")

        serverOpts <- list(
          desiredCapabilities =
            list(
              browserName = browserName,
              version = version,
              javascriptEnabled = javascript,
              platform = platform,
              nativeEvents = nativeEvents
            )
        )
        if (length(extraCapabilities) > 0) {
          serverOpts$desiredCapabilities <-
            c(
              serverOpts$desiredCapabilities,
              extraCapabilities
            )
        }
        qpath <- sprintf("%s/session", serverURL)
        queryRD(qpath, "POST", qdata = serverOpts)

        # fudge for sauceLabs not having /sessions
        sessionInfo <<- value
        # if (is.na(sessionid)) {
        #   # fix for problem with sauceLab when calling internet explorer
        #   sessionInfo$id <<- sub(".*hub/session/(.*)",
        #                          "\\1", responseheader$Location)
        #   sessionInfo <<- getSession()
        #   sessionInfo$id <<- sessionid
        # } else {
        sessionInfo$id <<- sessionid
        # }
        if (!silent) print(sessionInfo)
        #
      },
      getSessions = function() {
        "Returns a list of the currently active sessions. Each session
        will be returned as a list containing amongst other items:
        \\describe{
          \\item{\\code{id}:}{The session ID}
          \\item{\\code{capabilities}:}{An object describing session\'s
            capabilities}
        }"
        qpath <- sprintf("%s/sessions", serverURL)
        queryRD(qpath)
        .self$value
      },
      getSession = function() {
        qpath <- sprintf("%s/session/%s", serverURL, sessionInfo[["id"]])
        queryRD(qpath)
        .self$value
      },
      getStatus = function() {
        "Query the server\'s current status. All server implementations
        should return two basic objects describing the server\'s current
        platform and when the server was built."
        qpath <- sprintf("%s/status", serverURL)
        queryRD(qpath)
        .self$value
      },
      getAlertText = function() {
        "Gets the text of the currently displayed JavaScript alert(),
        confirm() or prompt() dialog."
        qpath <- sprintf(
          "%s/session/%s/alert_text",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },
      sendKeysToActiveElement = function(sendKeys) {
        "Send a sequence of key strokes to the active element. This
        command is similar to the send keys command in every aspect except
        the implicit termination: The modifiers are not released at the
        end of the call. Rather, the state of the modifier keys is kept
        between calls, so mouse interactions can be performed while
        modifier keys are depressed. The key strokes are sent as a list.
        Plain text is enter as an unnamed element of the list. Keyboard
        entries are defined in `selKeys` and should be listed with name
        `key`. See the examples."
        sendKeys <- list(value = matchSelKeys(sendKeys))
        qpath <- sprintf(
          "%s/session/%s/keys",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = sendKeys)
      },

      sendKeysToAlert = function(sendKeys) {
        "Sends keystrokes to a JavaScript prompt() or alert() dialog.
        The key strokes are sent as a list. Plain text is enter as an
        unnamed element of the list. Keyboard entries are defined in
        `selKeys` and should be listed with name `key`. See the examples."
        sendKeys <- list(
          text = paste(matchSelKeys(sendKeys), collapse = "")
        )
        qpath <- sprintf(
          "%s/session/%s/alert_text",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = sendKeys)
      },

      acceptAlert = function() {
        "Accepts the currently displayed alert dialog.  Usually, this is
        equivalent to clicking the 'OK' button in the dialog."
        qpath <- sprintf(
          "%s/session/%s/accept_alert",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST")
      },

      dismissAlert = function() {
        "Dismisses the currently displayed alert dialog. For confirm() and
        prompt() dialogs, this is equivalent to clicking the 'Cancel'
        button. For alert() dialogs, this is equivalent to clicking the
        'OK' button."
        qpath <- sprintf(
          "%s/session/%s/dismiss_alert",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST")
      },

      mouseMoveToLocation = function(
                                     x = NA_integer_,
                                     y = NA_integer_,
                                     webElement = NULL) {
        "Move the mouse by an offset of the specified element. If no
        element is specified, the move is relative to the current mouse
        cursor. If an element is provided but no offset, the mouse will be
        moved to the center of the element. If the element is not visible,
        it will be scrolled into view."
        if (!is.null(webElement)) {
          if (!is(webElement, "webElement")) {
            print("webElement should be of class webElement")
            return()
          }
          sendLoc <- list(element = as.character(webElement$elementId))
        } else {
          sendLoc <- list(element = NULL)
        }
        if (is.na(x)) {
          sendLoc <- sendLoc
        } else {
          sendLoc <- c(sendLoc, list(xoffset = as.integer(x)))
        }
        if (is.na(y)) {
          sendLoc <- sendLoc
        } else {
          sendLoc <- c(sendLoc, list(yoffset = as.integer(y)))
        }
        qpath <- sprintf(
          "%s/session/%s/moveto",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = sendLoc)
      },

      setAsyncScriptTimeout = function(milliseconds = 10000) {
        "Set the amount of time, in milliseconds, that asynchronous
        scripts executed by execute_async_script() are permitted to run
        before they are aborted and a |Timeout| error is returned to the
        client."
        qpath <- sprintf(
          "%s/session/%s/timeouts/async_script",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(ms = milliseconds))
      },

      setImplicitWaitTimeout = function(milliseconds = 10000) {
        "Set the amount of time the driver should wait when searching for
        elements. When searching for a single element, the driver will poll
        the page until an element is found or the timeout expires,
        whichever occurs first. When searching for multiple elements, the
        driver should poll the page until at least one element is found or
        the timeout expires, at which point it will return an empty list.
        If this method is never called, the driver will default to an
        implicit wait of 0ms."
        qpath <- sprintf(
          "%s/session/%s/timeouts/implicit_wait",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(ms = milliseconds))
      },

      setTimeout = function(type = "page load", milliseconds = 10000) {
        "Configure the amount of time that a particular type of operation
        can execute for before they are aborted and a |Timeout| error is
        returned to the client.
        \\describe{
          \\item{\\code{type}:}{The type of operation to set the timeout
            for. Valid values are: \"script\" for script timeouts,
            \"implicit\" for modifying the implicit wait timeout and
            \"page load\" for setting a page load timeout. Defaults to
            \"page load\" }
          \\item{\\code{milliseconds}:}{The amount of time, in
            milliseconds, that time-limited commands are permitted to run.
            Defaults to 10000 milliseconds. }
        }"
        qpath <- sprintf(
          "%s/session/%s/timeouts",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(
          qpath, "POST",
          qdata = list(type = type, ms = milliseconds)
        )
      },

      closeWindow = function() {
        "Close the current window."
        qpath <- sprintf(
          "%s/session/%s/window",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "DELETE")
      },

      close = function() {
        "Close the current session."
        qpath <- sprintf(
          "%s/session/%s",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "DELETE")
      },

      closeall = function() {
        getSessions()
        serverDetails <- value
        lapply(
          serverDetails,
          function(x) {
            qpath <- sprintf("%s/session/%s", serverURL, x[["id"]])
            queryRD(qpath, "DELETE")
          }
        )
      },

      quit = function() {
        "Delete the session & close open browsers."
        closeall()
      },

      getCurrentWindowHandle = function() {
        "Retrieve the current window handle."
        qpath <- sprintf(
          "%s/session/%s/window_handle",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      getWindowHandles = function() {
        "Retrieve the list of window handles used in the session."
        qpath <- sprintf(
          "%s/session/%s/window_handles",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      getWindowSize = function(windowId = "current") {
        "Retrieve the window size. `windowid` is optional (default is
        'current' window). Can pass an appropriate `handle`"
        qpath <- sprintf(
          "%s/session/%s/window/%s/size",
          serverURL, sessionInfo[["id"]], windowId
        )
        queryRD(qpath)
        .self$value
      },

      getWindowPosition = function(windowId = "current") {
        "Retrieve the window position. `windowid` is optional (default is
        'current' window). Can pass an appropriate `handle`"
        qpath <- sprintf(
          "%s/session/%s/window/%s/position",
          serverURL, sessionInfo[["id"]], windowId
        )
        queryRD(qpath)
        .self$value
      },

      getCurrentUrl = function() {
        "Retrieve the url of the current page."
        qpath <- sprintf(
          "%s/session/%s/url",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      navigate = function(url) {
        "Navigate to a given url."
        qpath <- sprintf(
          "%s/session/%s/url",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(url = url))
      },

      getTitle = function(url) {
        "Get the current page title."
        qpath <- sprintf(
          "%s/session/%s/title",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      goForward = function() {
        "Equivalent to hitting the forward button on the browser."
        qpath <- sprintf(
          "%s/session/%s/forward",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST")
      },

      goBack = function() {
        "Equivalent to hitting the back button on the browser."
        qpath <- sprintf(
          "%s/session/%s/back",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST")
      },

      refresh = function() {
        "Reload the current page."
        qpath <- sprintf(
          "%s/session/%s/refresh",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST")
      },

      executeAsyncScript = function(script, args = list()) {
        "Inject a snippet of JavaScript into the page for execution in the
        context of the currently selected frame. The executed script is
        assumed to be asynchronous and must signal that is done by
        invoking the provided callback, which is always provided as the
        final argument to the function. The value to this callback will be
        returned to the client. Asynchronous script commands may not span
        page loads. If an unload event is fired while waiting for a script
        result, an error should be returned to the client. "
        # change here to test class of args for a webElement
        if (any(lapply(args, class) == "webElement")) {
          # some of the function arguments are webElements
          args <- lapply(args, function(x) {
            if (is(x, "webElement")) {
              list(ELEMENT = x[["elementId"]])
            } else {
              x
            }
          })
        }
        qpath <- sprintf(
          "%s/session/%s/execute_async",
          serverURL, sessionInfo[["id"]]
        )

        if (.self$javascript) {
          queryRD(qpath, "POST", qdata = list(script = script, args = args))
        } else {
          "Javascript is not enabled"
        }
        if (length(.self$value) == 1) {
          if (any(names(.self$value) == "ELEMENT")) {
            return(
              if (inherits(.self, "webElement")) {
                list(
                  webElement$
                    new(as.character(.self$value))$
                    import(.self$export("remoteDriver"))
                )
              } else {
                list(
                  webElement$
                    new(as.character(.self$value))$
                    import(.self)
                )
              }
            )
          } else {
            return(.self$value)
          }
        }

        testWebElement(.self$value, .self)
      },

      executeScript = function(script, args = list("")) {
        "Inject a snippet of JavaScript into the page for execution in the
        context of the currently selected frame. The executed script is
        assumed to be synchronous and the result of evaluating the script
        is returned to the client. The script argument defines the script
        to execute in the form of a function body. The value returned by
        that function will be returned to the client. The function will be
        invoked with the provided args array and the values may be
        accessed via the arguments object in the order specified.
        Arguments may be any JSON-primitive, array, or JSON object. JSON
        objects that define a WebElement reference will be converted to
        the corresponding DOM element. Likewise, any WebElements in the
        script result will be returned to the client as WebElement JSON
        objects."
        # change here to test class of args for a webElement
        if (any(lapply(args, class) == "webElement")) {
          # some of the function arguments are webElements
          args <- lapply(args, function(x) {
            if (is(x, "webElement")) {
              list(ELEMENT = x[["elementId"]])
            } else {
              x
            }
          })
        }
        qpath <- sprintf(
          "%s/session/%s/execute",
          serverURL, sessionInfo[["id"]]
        )

        if (.self$javascript) {
          queryRD(qpath, "POST", qdata = list(script = script, args = args))
        } else {
          "Javascript is not enabled"
        }
        # if any of the returned elements are web Elements return them as
        # such
        if (length(.self$value) == 1) {
          if (any(names(.self$value) == "ELEMENT")) {
            return(
              if (inherits(.self, "webElement")) {
                list(
                  webElement$
                    new(as.character(.self$value))$
                    import(.self$export("remoteDriver"))
                )
              } else {
                list(
                  webElement$
                    new(as.character(.self$value))$
                    import(.self)
                )
              }
            )
          } else {
            return(.self$value)
          }
        }
        testWebElement(.self$value, .self)
      },

      screenshot = function(display = FALSE, useViewer = TRUE, file = NULL) {
        "Take a screenshot of the current page. The screenshot is returned
        as a base64 encoded PNG. If display is TRUE the screenshot is
        displayed locally. If useViewer is TRUE and RStudio is in use the
        screenshot is displayed in the RStudio viewer panel. If file is
        not NULL and display = FALSE the screenshot is written to the file
        denoted by file."
        qpath <- sprintf(
          "%s/session/%s/screenshot",
          serverURL, sessionInfo[["id"]]
        )

        queryRD(qpath)

        if (display) {
          tmp <- paste0(tempdir(), "/tmpScreenShot.png")
          writeBin(base64decode(.self$value[[1]], "raw"), tmp)
          viewer <- getOption("viewer")
          if (!is.null(viewer) && useViewer) {
            viewer(tmp)
          } else {
            utils::browseURL(tmp)
          }
        } else {
          if (is.null(file)) {
            .self$value
          } else {
            writeBin(base64decode(.self$value[[1]], "raw"), file)
          }
        }
      },
      switchToFrame = function(Id) {
        "Change focus to another frame on the page. Id can be
        string|number|null|WebElement Object. If the Id is null, the
        server should switch to the page's default content."
        if (inherits(Id, "webElement")) {
          # pass the webElement as Json to SS
          Id <- list("ELEMENT" = as.character(Id$elementId))
        }
        qpath <- sprintf(
          "%s/session/%s/frame",
          serverURL, sessionInfo[["id"]]
        )

        queryRD(qpath, "POST", qdata = list(id = Id))
      },

      switchToWindow = function(windowId) {
        "Change focus to another window. The window to change focus to may
        be specified by its server assigned window handle, or by the value
        of its name attribute."
        qpath <- sprintf(
          "%s/session/%s/window",
          serverURL, sessionInfo[["id"]]
        )
        if (browserName == "firefox") {
          qdata <- list(handle = windowId)
        } else {
          qdata <- list(name = windowId)
        }
        queryRD(qpath, "POST", qdata = qdata)
      },

      setWindowPosition = function(x, y, winHand = "current") {
        "Set the position (on screen) where you want your browser to be
        displayed. The windows handle is optional. If not specified the
        current window in focus is used."
        qpath <- sprintf(
          "%s/session/%s/window/%s/position",
          serverURL, sessionInfo[["id"]], winHand
        )
        queryRD(qpath, "POST", qdata = list(x = x, y = y))
      },

      setWindowSize = function(width, height, winHand = "current") {
        "Set the size of the browser window. The windows handle is
        optional. If not specified the current window in focus is used."
        qpath <- sprintf(
          "%s/session/%s/window/%s/size",
          serverURL, sessionInfo[["id"]], winHand
        )
        queryRD(
          qpath, "POST",
          qdata = list(width = width, height = height)
        )
      },

      maxWindowSize = function(winHand = "current") {
        "Set the size of the browser window to maximum. The windows handle
        is optional. If not specified the current window in focus is used."
        qpath <- sprintf(
          "%s/session/%s/window/%s/maximize",
          serverURL, sessionInfo[["id"]], winHand
        )
        queryRD(qpath, "POST")
      },

      getAllCookies = function() {
        " Retrieve all cookies visible to the current page. Each cookie
        will be returned as a list with the following name and value types:
        \\describe{
          \\item{\\code{name}:}{character}
          \\item{\\code{value}:}{character}
          \\item{\\code{path}:}{character}
          \\item{\\code{domain}:}{character}
          \\item{\\code{secure}:}{logical}
        }"
        qpath <- sprintf(
          "%s/session/%s/cookie",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      addCookie = function(
                           name,
                           value,
                           path = "/",
                           domain = NULL,
                           httpOnly = NULL,
                           expiry = NULL,
                           secure = FALSE) {
        "Set a cookie on the domain. The inputs are required apart from
        those with default values."
        cookie <- list(
          name = name,
          value = value,
          path = path,
          domain = domain,
          httpOnly = httpOnly,
          expiry = expiry,
          secure = secure
        )
        cookie <- cookie[!vapply(cookie, is.null, logical(1))]
        qpath <- sprintf(
          "%s/session/%s/cookie",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(cookie = cookie))
      },

      deleteAllCookies = function() {
        "Delete all cookies visible to the current page."
        qpath <- sprintf(
          "%s/session/%s/cookie",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "DELETE")
      },

      deleteCookieNamed = function(name) {
        "Delete the cookie with the given name. This command will be a
        no-op if there is no such cookie visible to the current page."
        qpath <- sprintf(
          "%s/session/%s/cookie/%s",
          serverURL, sessionInfo[["id"]], name
        )
        queryRD(qpath, "DELETE")
      },

      getPageSource = function(...) {
        "Get the current page source."
        qpath <- sprintf(
          "%s/session/%s/source",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      findElement = function(
                             using = c(
                               "xpath", "css selector", "id", "name", "tag name", "class name",
                               "link text", "partial link text"
                             ),
                             value) {
        "Search for an element on the page, starting from the document
        root. The located element will be returned as an object of
        webElement class.The inputs are:
        \\describe{
          \\item{\\code{using}:}{Locator scheme to use to search the
            element, available schemes: Defaults to 'xpath'. Partial
            string matching is accepted.
            \\describe{
              \\item{\"class name\" :}{Returns an element whose class name
                contains the search value; compound class names are not
                permitted.}
              \\item{\"css selector\" :}{Returns an element matching a CSS
                selector.}
              \\item{\"id\" :}{Returns an element whose ID attribute
                matches the search value.}
              \\item{\"name\" :}{Returns an element whose NAME attribute
                matches the search value.}
              \\item{\"link text\" :}{Returns an anchor element whose
                visible text matches the search value.}
              \\item{\"partial link text\" :}{Returns an anchor element
                whose visible text partially matches the search value.}
              \\item{\"tag name\" :}{Returns an element whose tag name
                matches the search value.}
              \\item{\"xpath\" :}{Returns an element matching an XPath
                expression.}
            }
          }
          \\item{\\code{value}:}{The search target. See examples.}
        }"
        using <- match.arg(using)
        qpath <- sprintf(
          "%s/session/%s/element",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(using = using, value = value))

        # using value as an argument refer to self
        elemDetails <- .self$value[[1]]
        webElement$
          new(as.character(elemDetails))$
          import(.self)
      },

      findElements = function(
                              using = c(
                                "xpath", "css selector", "id", "name", "tag name", "class name",
                                "link text", "partial link text"
                              ),
                              value) {
        "Search for multiple elements on the page, starting from the
        document root. The located elements will be returned as an list of
        objects of class WebElement. The inputs are:
        \\describe{
          \\item{\\code{using}:}{Locator scheme to use to search the
          element, available schemes: {\"class name\", \"css selector\",
          \"id\", \"name\", \"link text\", \"partial link text\",
          \"tag name\", \"xpath\" }. Defaults to 'xpath'. Partial string
          matching is accepted. See the findElement method for details}
          \\item{\\code{value}:}{The search target. See examples.}
        }"
        using <- match.arg(using)
        qpath <- sprintf(
          "%s/session/%s/elements",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(using = using, value = value))
        elemDetails <- .self$value
        lapply(
          elemDetails,
          function(x) {
            webElement$
              new(as.character(x))$
              import(.self)
          }
        )
      },

      getActiveElement = function() {
        "Get the element on the page that currently has focus. The located
        element will be returned as a WebElement id."
        qpath <- sprintf(
          "%s/session/%s/element/active",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      click = function(buttonId = 0) {
        "Click any mouse button (at the coordinates set by the last
        mouseMoveToLocation() command). buttonId - any one of 'LEFT'/0
        'MIDDLE'/1 'RIGHT'/2. Defaults to 'LEFT'"
        qpath <- sprintf(
          "%s/session/%s/click",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(button = buttonId))
      },

      doubleclick = function(buttonId = 0) {
        "Double-Click any mouse button (at the coordinates set by the last
        mouseMoveToLocation() command). buttonId - any one of 'LEFT'/0
        'MIDDLE'/1 'RIGHT'/2. Defaults to 'LEFT'"
        qpath <- sprintf(
          "%s/session/%s/doubleclick",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(button = buttonId))
      },

      buttondown = function(buttonId = 0) {
        "Click and hold the given mouse button (at the coordinates set by
        the last moveto command). Note that the next mouse-related command
        that should follow is buttondown . Any other mouse command (such
        as click or another call to buttondown) will yield undefined
        behaviour. buttonId - any one of 'LEFT'/0 'MIDDLE'/1 'RIGHT'/2.
        Defaults to 'LEFT'"
        qpath <- sprintf(
          "%s/session/%s/buttondown",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(button = buttonId))
      },

      buttonup = function(buttonId = 0) {
        "Releases the mouse button previously held (where the mouse is
        currently at). Must be called once for every buttondown command
        issued. See the note in click and buttondown about implications of
        out-of-order commands. buttonId - any one of 'LEFT'/0 'MIDDLE'/1
        'RIGHT'/2. Defaults to 'LEFT'"
        qpath <- sprintf(
          "%s/session/%s/buttonup",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(button = buttonId))
      },

      getLogTypes = function() {
        "Get available log types. Common log types include 'client' = Logs
        from the client, 'driver' = Logs from the webdriver, 'browser' =
        Logs from the browser, 'server' = Logs from the server. Other log
        types, for instance, for performance logging may also be
        available. phantomjs for example returns a har log type which is a
        single-entry log, with the HAR (HTTP Archive) of the current
        webpage, since the first load (it's cleared at every unload event)"
        qpath <- sprintf(
          "%s/session/%s/log/types",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath)
        .self$value
      },

      log = function(type) {
        "Get the log for a given log type. Log buffer is reset after each
        request.
        \\describe{
          \\item{\\code{type}:}{The log type. Typically 'client', 'driver',
            'browser', 'server'}
        }
        "
        qpath <- sprintf(
          "%s/session/%s/log",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(type = type))
        .self$value
      },

      phantomExecute = function(script, args = list()) {
        "This API allows you to send a string of JavaScript via 'script',
        written for PhantomJS, and be interpreted within the context of a
        WebDriver Page. In other words, for the given script then this
        variable is initialized to be the current Page. See
        \\url{https://github.com/ariya/phantomjs/wiki/API-Reference-WebPage}
        and the example in this help file. NOTE: Calling the PhantomJS API
        currently only works when PhantomJS is driven directly via
        \\code{\\link{phantom}}"
        qpath <- sprintf(
          "%s/session/%s/phantom/execute",
          serverURL, sessionInfo[["id"]]
        )
        queryRD(qpath, "POST", qdata = list(script = script, args = args))
        .self$value
      },

      closeServer = function() {
        "Closes the server in practice terminating the process. This is
        useful for linux systems. On windows the java binary operates as a
        separate shell which the user can terminate."
        servURL <- httr::parse_url(serverURL)
        servURL[["path"]] <-
          "/selenium-server/driver/?cmd=shutDownSeleniumServer"
        httr::content(
          httr::GET(httr::build_url(servURL)),
          encoding = "UTF-8"
        )
      }
    )
  )
