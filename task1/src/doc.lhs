% Note: this file contains no source code; it is only used to generate
% documentation. Process it using lhs2tex.

\documentclass[a4paper,DIV15]{scrartcl}

%include polycode.fmt
\newcommand{\intdiv}{\mathbin{/\!\!/}}
\newcommand{\testlabel}{\mathbin{\sim :}}
\newcommand{\testequal}{\mathbin{\sim\stackrel{?}{=}}}
%format //  = "\intdiv "
%format ~:  = "\testlabel "
%format ~=? = "\testequal "
%format qualified  = "\mathbf{qualified}"
%format as  = "\mathbf{as}"

\usepackage{booktabs}
\usepackage[sc]{mathpazo}
\usepackage{tgpagella}
\usepackage[T1]{fontenc}

\title{Cakeulator: A Stack-Based Calculator in Haskell}

\setlength{\parskip}{2pt plus 1pt}

\begin{document}

\section{Display}

%include CakeDisplay.lhs

\section{Value}

%include CakeValue.lhs

\section{Stack}

%include CakeStack.lhs

\section{State}

%include CakeState.lhs

\section{Parser}

%include CakeParser.lhs

\section{Printer}

%include CakePrinter.lhs

\section{Evaluator}

%include CakeEvaluator.lhs

\section{I/O (Main)}

%include Main.lhs

\end{document}
