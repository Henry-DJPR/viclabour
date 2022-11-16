<script>
  //Imports
  import { onMount } from 'svelte'
  import Highcharts from 'highcharts'
  import accessibility from 'highcharts/modules/accessibility'
  import exporting from 'highcharts/modules/exporting'
  import exportdata from 'highcharts/modules/export-data'
  import stock from 'highcharts/modules/stock'
  import seriesLabel from 'highcharts/modules/series-label'

  accessibility(Highcharts)
  exporting(Highcharts)
  exportdata(Highcharts)
  stock(Highcharts)
  seriesLabel(Highcharts)

  //Define props
  export let chartId = 'test'
  export let chartType = 'normal'
  export let height = '500px'
  export let width = '100%'
  export let containerClass = ''

  //Prop transformations
  if (chartType == 'stock') {
    chartType = 'stockChart'
  } else {
    chartType = 'chart'
  }
  let style = `height: ${height}; width: ${width};`
  let jsonURL = new URL(
    'highcharts_data/' + chartId + '.json',
    window.location.href
  )

  // Chart defults
  Highcharts.setOptions({
    chart: {
      styledMode: true
    },
    credits: {
      enabled: false
    },
    title: {
      align: 'left'
    },
    subtitle: {
      align: 'left'
    },
    yAxis: {
      opposite: false
    }
  })

  // Async generate chart
  onMount(async () => {
    Highcharts.getJSON(jsonURL.toString(), function (data) {
      Highcharts[chartType](chartId, data)
    })
  })
</script>

<div id={chartId} {style} class={containerClass} />
