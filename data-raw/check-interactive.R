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
        return {hover: true, zoom: true, search: true, sort: true,
          pagination: true, embeddedResources: true};
      })()
    }",
    awaitPromise = TRUE,
    returnByValue = TRUE
  )
  if (!is.null(result$exceptionDetails)) {
    stop(result$exceptionDetails$exception$description, call. = FALSE)
  }
  print(result$result$value)
  invisible(result$result$value)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript data-raw/check-interactive.R path/to/interactive.html")
}
check_interactive_report(args[[1]])
