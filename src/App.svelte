<script>
  import Sidebar from './lib/Sidebar.svelte'
  import SidebarToggle from './lib/SidebarToggle.svelte'
  import PageOverview from './lib/PageOverview.svelte'
  import BadgeLatest from './lib/BadgeLatest.svelte'
  import BadgeNext from './lib/BadgeNext.svelte'
</script>

<Sidebar />
<main>
  <div class="container py-4 px-3">
    <h1 class="my-4 display-3 text-center text-md-start">
      DJPR labour dashboard
    </h1>
    <div class="row gy-2">
      <div class="col-sm">
        <SidebarToggle />
      </div>
      <div class="col-sm">
        <BadgeLatest />
      </div>
      <div class="col-sm">
        <BadgeNext />
      </div>
    </div>

    <hr class="mb-5" />
    <PageOverview />
  </div>
</main>
