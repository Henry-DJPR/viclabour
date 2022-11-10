<script>
  //Imports
  import { onMount } from 'svelte'
  import Highcharts from 'highcharts'
  import accessibility from 'highcharts/modules/accessibility'
  import exporting from 'highcharts/modules/exporting'
  import exportdata from 'highcharts/modules/export-data'
  import stock from 'highcharts/modules/stock'
  import hcData from '../assets/highcharts_data/test.json'

  accessibility(Highcharts)
  exporting(Highcharts)
  exportdata(Highcharts)
  stock(Highcharts)

  //Define props
  export let jsonPath = new URL(
    'highcharts_data/test.json',
    window.location.href
  )
  export let chartType = 'normal'
  export let height = '500px'
  export let width = '100%'

  //Prop transformations
  if (chartType == 'stock') {
    chartType = 'stockChart'
  } else {
    chartType = 'chart'
  }
  let style = `height: ${height}; width: ${width};`

  // Async generate chart
  onMount(async () => {
    Highcharts.getJSON(jsonPath.toString(), function (data) {
      Highcharts[chartType]('chartHolster', data)
    })
  })
</script>

<p>Module loaded</p>
<div id="chartHolster" {style} />
