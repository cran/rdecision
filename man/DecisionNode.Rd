% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DecisionNode.R
\docType{class}
\name{DecisionNode}
\alias{DecisionNode}
\title{A decision node in a decision tree}
\description{
An R6 class representing a decision node in a decision tree.
}
\details{
A class to represent a decision node in a decision tree. The node
is associated with one or more branches to child nodes. Inherits from class
\code{Node}.
}
\author{
Andrew J. Sims \email{andrew.sims@newcastle.ac.uk}
}
\section{Super class}{
\code{\link[rdecision:Node]{rdecision::Node}} -> \code{DecisionNode}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DecisionNode-new}{\code{DecisionNode$new()}}
\item \href{#method-DecisionNode-clone}{\code{DecisionNode$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Node" data-id="label"><a href='../../rdecision/html/Node.html#method-Node-label'><code>rdecision::Node$label()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Node" data-id="type"><a href='../../rdecision/html/Node.html#method-Node-type'><code>rdecision::Node$type()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DecisionNode-new"></a>}}
\if{latex}{\out{\hypertarget{method-DecisionNode-new}{}}}
\subsection{Method \code{new()}}{
Create a new decision node.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionNode$new(label)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{label}}{A label for the node. Must be defined because the label is
used in tabulation of strategies. The label is automatically converted
to a syntactically valid (in R) name to ensure it can be used as a column
name in a data frame.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{DecisionNode} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DecisionNode-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DecisionNode-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionNode$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
