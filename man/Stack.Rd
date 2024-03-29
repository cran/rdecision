% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stack.R
\docType{class}
\name{Stack}
\alias{Stack}
\title{A stack}
\description{
An R6 class representing a stack of objects of any type.
}
\details{
Conventional implementation of a stack. Used extensively in graph
algorithms and offered as a separate class for ease of programming and to
ensure that implementations of stacks are optimized. By intention, there is
only minimal checking of method arguments. This is to maximize performance
and because the class is mainly intended for use internally to
\pkg{rdecision}.
}
\author{
Andrew Sims \email{andrew.sims@newcastle.ac.uk}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Stack-new}{\code{Stack$new()}}
\item \href{#method-Stack-push}{\code{Stack$push()}}
\item \href{#method-Stack-pop}{\code{Stack$pop()}}
\item \href{#method-Stack-size}{\code{Stack$size()}}
\item \href{#method-Stack-as_list}{\code{Stack$as_list()}}
\item \href{#method-Stack-clone}{\code{Stack$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Stack-new"></a>}}
\if{latex}{\out{\hypertarget{method-Stack-new}{}}}
\subsection{Method \code{new()}}{
Create a stack.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Stack$new()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A new \code{Stack} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Stack-push"></a>}}
\if{latex}{\out{\hypertarget{method-Stack-push}{}}}
\subsection{Method \code{push()}}{
Push an item onto the stack.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Stack$push(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{The item to push onto the top of the stack. It should be
of the same class as items previously pushed on to the stack. It is not
checked.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An updated \code{Stack} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Stack-pop"></a>}}
\if{latex}{\out{\hypertarget{method-Stack-pop}{}}}
\subsection{Method \code{pop()}}{
Pop an item from the stack. Stack underflow and
raises an error.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Stack$pop()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The item previously at the top of the stack.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Stack-size"></a>}}
\if{latex}{\out{\hypertarget{method-Stack-size}{}}}
\subsection{Method \code{size()}}{
Gets the number of items on the stack.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Stack$size()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Number of items.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Stack-as_list"></a>}}
\if{latex}{\out{\hypertarget{method-Stack-as_list}{}}}
\subsection{Method \code{as_list()}}{
Inspect items in the stack.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Stack$as_list()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of items.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Stack-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Stack-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Stack$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
