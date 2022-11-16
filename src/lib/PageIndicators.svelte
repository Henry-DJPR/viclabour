<script>
  import TableIndicators from './TableIndicators.svelte'
  import Highchart from './Highchart.svelte'
</script>

<h2>Indicators</h2>
<p>
  This page includes headline estimates of employment, unemployment,
  underemployment, participation, and hours worked for Victoria. The page also
  provides state-based comparison for key labour force indicators.
</p>
<div class="table-responsive my-5">
  <TableIndicators />
</div>
<h3>Employment</h3>
<Highchart
  chartType="stock"
  chartId="viz_ind_emppop_state_line"
  containerClass="my-5"
  height="550px"
/>
<Highchart
  chartType="stock"
  chartId="viz_ind_empgro_line"
  containerClass="my-5"
/>
<div class="row g-5">
  <div class="col-md-6">
    <Highchart chartType="stock" chartId="viz_ind_gen_full_part_line" />
  </div>
  <div class="col-md-6">
    <Highchart chartType="stock" chartId="test" />
  </div>
</div>
