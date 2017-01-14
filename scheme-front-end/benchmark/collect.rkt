#lang racket
(require "suite.rkt" file/glob)

(module+ main
  (define path/benchmark/results "results")

  (define timeout 1800)

  (define results
    (for/list ([test suite])
      (match-define (Test name test-cases) test)
      (define measurements
        (for/list ([test-case test-cases])
          (match-define `(,analysis . ,(TestCase k _)) test-case)
          (define path/benchmark/result
            (~a path/benchmark/results "/"
                "*" "--analysis=" analysis "--name=" name "--k=" k ".txt"))
          (define results/files
            (sort (map path->string (glob path/benchmark/result)) string<?))
          (when (empty? results/files)
            (raise-user-error "Can’t find result: " test-case))
          (define result/text (file->string (last results/files)))
          (define time/raw
            (case analysis
              [(ddpa)
               (car (regexp-match* #px"(?m:^real (.*)$)" result/text #:match-select cadr))]
              [(p4f)
               (exact->inexact
                (/
                 (string->number
                  (car (regexp-match* #px"(?m:^Analysis run for: (.*) milliseconds$)"
                                      result/text
                                      #:match-select cadr)))
                 1000))]))
          (define time (if (>= time/raw timeout) timeout time/raw))
          `(,analysis . ,time)))
      (Result test measurements)))

  (display (~a
            "\\begin{figure}
  \\begin{center}
    \\begin{small}
      \\begin{tabular}{l|rr|rr}
        &\\multicolumn{2}{c|}{\\textbf{DDPA}}&\\multicolumn{2}{c|}{\\textbf{P4F}}\\\\
        \\textbf{Program}&\\textbf{$k$}&\\textbf{Time (s)}&\\textbf{$k$}&\\textbf{Time (s)}\\\\
        \\hline
"
            (string-join
             (for/list ([result results])
               (match-define (Result (Test name test-cases) measurements) result)
               (match-define (TestCase ddpa/k _) (dict-ref test-cases 'ddpa))
               (define ddpa/time (dict-ref measurements 'ddpa))
               (define p4f/k
                 (match (dict-ref test-cases 'p4f #f)
                   [(TestCase p4f/k _) p4f/k]
                   [#f "--"]))
               (define p4f/time/maybe (dict-ref measurements 'p4f #f))
               (define p4f/time
                 (cond
                   [(and p4f/time/maybe (< p4f/time/maybe timeout)) p4f/time/maybe]
                   [(and p4f/time/maybe (>= p4f/time/maybe timeout)) "\\emph{t}"]
                   [(not p4f/time/maybe) "--"]))
               (~a "\\emph{" name "}&" ddpa/k "&" ddpa/time "&" p4f/k "&" p4f/time))
             "\\\\\n")
            "
      \\end{tabular}
    \\end{small}

    \\vspace{1em}

    \\definecolor{DDPAcolor}{HTML}{332288}
    \\definecolor{P4Fcolor}{HTML}{DD88AA}
    \\begin{tikzpicture}
      \\begin{axis}[
        width = \\textwidth,
        height = 6cm,
        major x tick style = transparent,
        ybar = 3*\\pgflinewidth,
        bar width = 0.8em,
        ymajorgrids = true,
        ylabel = {Time (s)},
        symbolic x coords={"
            (string-join
             (for/list ([result results])
               (match-define (Result (Test name _) _) result)
               (~a name))
             ",")
            "},
        xtick = data,
        x tick label style={rotate=45,anchor=north east},
        scaled y ticks = false,
        restrict y to domain*=0:10,
        enlarge x limits = 0.05,
        ymin = 0,
        ymax = 10,
        legend cell align=left,
        legend style={
          anchor=north west,
          at={(0.3,1)}
        },
        visualization depends on=rawy\\as\\rawy,
        after end axis/.code={ % Draw line indicating break
          \\draw [ultra thick, white, decoration={snake, amplitude=1pt}, decorate] (rel axis cs:0,0.95) -- (rel axis cs:1,0.95);
        },
        nodes near coords={
          \\tiny\\pgfmathprintnumber[fixed]{\\rawy}
        },
        nodes near coords style={rotate=90,anchor=west},
        every tick label/.append style={
          font=\\footnotesize\\em,
          text height=1.25ex,
          text depth=.25ex,
          text centered,
        },
        clip=false,
        ]

        \\addplot[style={DDPAcolor,fill=DDPAcolor}]
        coordinates {
"
            (string-join
             (for/list ([result results])
               (match-define (Result (Test name test-cases) measurements) result)
               (match-define (TestCase ddpa/k _) (dict-ref test-cases 'ddpa))
               (define ddpa/time (dict-ref measurements 'ddpa))
               (~a "(" name ", " ddpa/time ")"))
             "\n")
            "
        };
        \\addplot[style={P4Fcolor,fill=P4Fcolor}]
        coordinates {
"
            (string-join
             (for/list ([result results])
               (match-define (Result (Test name test-cases) measurements) result)
               (match (dict-ref test-cases 'p4f #f)
                 [(TestCase p4f/k _)
                  (define p4f/time (dict-ref measurements 'p4f))
                  (~a "(" name ", " p4f/time ")")]
                 [#f ""]))
             "\n")
            "
        };
        \\legend{DDPA,P4F}
      \\end{axis}
    \\end{tikzpicture}
  \\end{center}

  \\caption{Results of the benchmark.  \\emph{t} means the execution timed-out after 30 minutes.}
  \\label{fig:benchmark-results}
\\end{figure}
")))