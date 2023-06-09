% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webElement.R
\docType{class}
\name{webElement-class}
\alias{webElement-class}
\alias{webElement}
\title{CLASS webElement}
\description{
Selenium Webdriver represents all the HTML elements as WebElements.
   This class provides a mechanism to represent them as objects &
   perform various actions on the related elements. Typically, the
   findElement method in \code{\link{remoteDriver}} returns an object
   of class webElement.
}
\details{
webElement is a generator object. To define a new webElement class
   method `new` is called.  When a webElement class is created an
   elementId should be given. Each webElement inherits from a
   remoteDriver. webElement is not usually called by the end-user.
}
\section{Fields}{

\describe{
\item{\code{elementId}}{Object of class \code{"character"}, giving a character
representation of the element id.}
}}

\section{Methods}{

\describe{
\item{\code{clearElement()}}{Clear a TEXTAREA or text INPUT element's value.}

\item{\code{clickElement()}}{Click the element.}

\item{\code{compareElements(otherElem)}}{Test if the current webElement and an other web element refer to
the same DOM element.}

\item{\code{describeElement()}}{Describe the identified element.}

\item{\code{findChildElement(
  using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text",
    "partial link text"),
  value
)}}{Search for an element on the page, starting from the node defined
by the parent webElement. The located element will be returned as
an object of webElement class.
The inputs are:
\describe{
  \item{\code{using}:}{Locator scheme to use to search the
    element, available schemes: {"class name", "css selector",
    "id", "name", "link text", "partial link text",
    "tag name", "xpath" }. Defaults to 'xpath'. Partial string
    matching is accepted.}
  \item{\code{value}:}{The search target. See examples.}
}}

\item{\code{findChildElements(
  using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text",
    "partial link text"),
  value
)}}{Search for multiple elements on the page, starting from the node
defined by the parent webElement. The located elements will be
returned as an list of objects of class WebElement.
The inputs are:
\describe{
  \item{\code{using}:}{Locator scheme to use to search the
    element, available schemes: {"class name", "css selector",
    "id", "name", "link text", "partial link text",
    "tag name", "xpath" }. Defaults to 'xpath'.
    Partial string matching is accepted.}
  \item{\code{value}:}{The search target. See examples.}
}}

\item{\code{getElementAttribute(attrName)}}{Get the value of an element's attribute. See examples.}

\item{\code{getElementLocation()}}{Determine an element's location on the page. The point (0, 0)
refers to the upper-left corner of the page.}

\item{\code{getElementLocationInView()}}{Determine an element's location on the screen once it has been
scrolled into view.
Note: This is considered an internal command and should only be
used to determine an element's location for correctly generating
native events.}

\item{\code{getElementSize()}}{Determine an element's size in pixels. The size will be returned
with width and height properties.}

\item{\code{getElementTagName()}}{Query for an element's tag name.}

\item{\code{getElementText()}}{Get the innerText of the element.}

\item{\code{getElementValueOfCssProperty(propName)}}{Query the value of an element's computed CSS property. The CSS
property to query should be specified using the CSS property name,
not the JavaScript property name (e.g. background-color instead of
backgroundColor).}

\item{\code{highlightElement(wait = 75/1000)}}{Utility function to highlight current Element. Wait denotes the
time in seconds between style changes on element.}

\item{\code{isElementDisplayed()}}{Determine if an element is currently displayed.}

\item{\code{isElementEnabled()}}{Determine if an element is currently enabled. Obviously to enable
an element just preform a click on it.}

\item{\code{isElementSelected()}}{Determine if an OPTION element, or an INPUT element of type
checkbox or radiobutton is currently selected.}

\item{\code{selectTag()}}{Utility function to return options from a select DOM node. The
option nodes are returned as webElements. The option text and the
value of the option attribute 'value' and whether the option is
selected are returned also. If this
method is called on a webElement that is not a select DOM node an
error will result.}

\item{\code{sendKeysToElement(sendKeys)}}{Send a sequence of key strokes to an element. The key strokes are
sent as a list. Plain text is enter as an unnamed element of the
list. Keyboard entries are defined in `selKeys` and should be
listed with name `key`. See the examples.}

\item{\code{setElementAttribute(attributeName, value)}}{Utility function to set an elements attributes.}

\item{\code{submitElement()}}{Submit a FORM element. The submit command may also be applied to
any element that is a descendant of a FORM element.}
}}

