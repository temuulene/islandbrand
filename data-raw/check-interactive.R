# Run after rendering inst/examples/interactive.qmd. Requires chromote and
# Chrome or Chromium, in addition to the package's interactive dependencies.
check_interactive_report <- function(path) {
  session <- chromote::ChromoteSession$new(width = 1000, height = 1100)
  on.exit(session$close(), add = TRUE)
  url <- paste0(
    "file:///",
    utils::URLencode(normalizePath(path, winslash = "/"))
  )
  session$Page$navigate(url)
  result <- session$Runtime$evaluate(
    expression = r"{
      (async () => {
        const wait = () => new Promise(resolve => setTimeout(resolve, 50));
        for (let i = 0; i < 200; i++) {
          if (document.querySelector('.js-plotly-plot')?.data &&
              document.querySelector('.rt-tbody .rt-tr')) break;
          await wait();
        }
        const assert = (condition, message) => {
          if (!condition) throw new Error(message);
        };
        const plot = document.querySelector('.js-plotly-plot');
        assert(plot?.data?.length > 0, 'Plot did not initialise');
        assert(document.querySelector('.islh-widget-heading'),
          'Missing heading');
        assert(document.querySelector('.islh-widget-note'),
          'Missing source note');
        const bars = plot.data.findIndex(trace => trace.type === 'bar');
        assert(bars >= 0, 'Missing epidemic curve bars');
        assert(plot.data[bars].y[5] === 52, 'Counts changed during conversion');
        Plotly.Fx.hover(plot, [{curveNumber: bars, pointNumber: 5}]);
        await wait();
        const hover = document.querySelector('.hoverlayer')?.textContent;
        assert(hover?.includes('Date: 2026-02-08'),
          'Hover must show the original reporting date');
        assert(hover?.includes('Cases: 52'), 'Hover count is incorrect');
        assert(!/\.islh_|group: 1/.test(hover),
          'Hover exposes internal plotting fields');
        await Plotly.relayout(plot,
          {'xaxis.range': ['2026-02-01', '2026-02-28']});
        assert(plot.layout.xaxis.range[0].startsWith('2026-02-01'),
          'Zoom failed');
        await Plotly.relayout(plot, {'xaxis.autorange': true});

        const table = document.querySelector('.reactable');
        const rows = () => Array.from(
          table.querySelectorAll('.rt-tbody .rt-tr'))
          .filter(row => row.textContent.trim());
        assert(rows().length === 5, 'Page size was not applied');
        const next = table.querySelector('button[aria-label="Next page"]');
        assert(next && !next.disabled, 'Next page is unavailable');
        next.click();
        await wait();
        assert(rows()[0].textContent.includes('52'), 'Pagination failed');

        const input = table.querySelector('input[aria-label="Search"]');
        assert(input, 'Search is missing');
        const setter = Object.getOwnPropertyDescriptor(
          HTMLInputElement.prototype, 'value').set;
        setter.call(input, '52');
        input.dispatchEvent(new Event('input', {bubbles: true}));
        for (let i = 0; i < 40 && rows().length !== 1; i++) await wait();
        assert(rows().length === 1 && rows()[0].textContent.includes('52'),
          'Search failed');
        setter.call(input, '');
        input.dispatchEvent(new Event('input', {bubbles: true}));
        for (let i = 0; i < 40 && rows().length !== 5; i++) await wait();

        const header = Array.from(
          table.querySelectorAll('[role="columnheader"]'))
          .find(node => node.textContent.includes('Reported cases'));
        assert(header, 'Sortable header is missing');
        header.click();
        await wait();
        const counts = rows().map(row => Number(
          row.querySelectorAll('.rt-td')[1].textContent));
        assert(counts.every((n, i) => i === 0 || n >= counts[i - 1]),
          'Numeric sorting failed');
        const resources = document.querySelectorAll(
          'script[src],link[rel="stylesheet"]');
        assert(!Array.from(resources)
          .some(node => /^https?:/.test(node.src || node.href)),
          'Report depends on external scripts or styles');

        const safe = document.querySelectorAll('.reactable')[1];
        assert(safe, 'Suppression example is missing');
        const caseHeader = Array.from(
          safe.querySelectorAll('[role="columnheader"]'))
          .find(node => node.textContent.includes('cases'));
        caseHeader.click();
        await wait();
        const safeValues = Array.from(safe.querySelectorAll('.rt-tbody .rt-tr'))
          .map(row => row.querySelectorAll('.rt-td')[1]?.textContent)
          .filter(Boolean);
        assert(safeValues.includes('Suppressed'),
          'Suppressed label is missing');
        assert(safeValues.filter(value => value !== 'Suppressed').join(',') ===
          '17,42,105', 'Suppressed counts no longer sort numerically');
        const customised = document.querySelectorAll('.js-plotly-plot')[3];
        assert(customised?.layout.xaxis.tickangle === -45,
          'Partial theme tweak was lost');
        assert(customised.layout.xaxis.showgrid === false,
          'Partial theme tweak removed the brand styling');
        return {hover: true, zoom: true, search: true, sort: true,
          pagination: true, embeddedResources: true, suppression: true,
          partialTheme: true};
      })()
    }",
    awaitPromise = TRUE,
    returnByValue = TRUE
  )
  if (!is.null(result$exceptionDetails)) {
    stop(result$exceptionDetails$exception$description, call. = FALSE)
  }

  # Area hovers use a different browser path from Plotly.Fx.hover(). Move the
  # pointer over a real tile in both the plain and grouped case curves.
  for (index in 1:2) {
    target <- session$Runtime$evaluate(
      expression = sprintf(
        r"{
        (() => {
          const plot = document.querySelectorAll('.js-plotly-plot')[%d];
          if (!plot) throw new Error('Case curve is missing');
          plot.scrollIntoView({block: 'center'});
          const tile = plot.querySelector('.scatterlayer .trace .js-fill');
          if (!tile) throw new Error('Case tile is missing');
          const box = tile.getBoundingClientRect();
          return {x: box.x + box.width / 2, y: box.y + box.height / 2};
        })()
      }",
        index
      ),
      returnByValue = TRUE
    )
    if (!is.null(target$exceptionDetails)) {
      stop(target$exceptionDetails$exception$description, call. = FALSE)
    }
    point <- target$result$value
    session$Input$dispatchMouseEvent(
      type = "mouseMoved",
      x = point$x,
      y = point$y
    )
    hover <- session$Runtime$evaluate(
      expression = sprintf(
        r"{
        (async () => {
          const plot = document.querySelectorAll('.js-plotly-plot')[%d];
          for (let i = 0; i < 40; i++) {
            const text = plot.querySelector('.hoverlayer')?.textContent || '';
            if (text.includes('Date: 2026-01-04') &&
                text.includes('Cases: 2')) {
              if (%d === 2 && !text.includes('area: North')) {
                throw new Error('Case hover group is incorrect: ' + text);
              }
              if ((text.match(/area: North/g) || []).length > 1) {
                throw new Error('Case hover repeats its facet field: ' + text);
              }
              if (/\.islh_|trace \d/.test(text)) {
                throw new Error('Case hover exposes internal fields: ' + text);
              }
              return true;
            }
            await new Promise(resolve => setTimeout(resolve, 50));
          }
          throw new Error('Case hover must show reporting date and count: ' +
            plot.querySelector('.hoverlayer')?.textContent);
        })()
      }",
        index,
        index
      ),
      awaitPromise = TRUE,
      returnByValue = TRUE
    )
    if (!is.null(hover$exceptionDetails)) {
      stop(hover$exceptionDetails$exception$description, call. = FALSE)
    }
  }
  result$result$value$caseTiles <- TRUE
  print(result$result$value)
  invisible(result$result$value)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript data-raw/check-interactive.R path/to/interactive.html")
}
check_interactive_report(args[[1]])
