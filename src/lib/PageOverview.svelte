<script>
  import TableOverview from './TableOverview.svelte'
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
