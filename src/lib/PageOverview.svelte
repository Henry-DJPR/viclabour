<script>
  import TableOverview from './TableOverview.svelte'
  import Highchart from './Highchart.svelte'
</script>

<p>
  The Victorian jobs dashboard explores information on Victorian employment and
  job seekers. Use the <a
    data-bs-toggle="offcanvas"
    href="#sidebarNav"
    aria-controls="sidebarNav"
  >
    demographics menu
  </a> for detailed breakdowns of Victorian labour.
</p>
<div class="table-responsive my-5">
  <TableOverview />
</div>

<h2>Highchart test</h2>
<Highchart chartType="stock" />
