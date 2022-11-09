<script>
  import { Tab } from '../node_modules/bootstrap'
  import Sidebar from './lib/Sidebar.svelte'
  import SidebarToggle from './lib/SidebarToggle.svelte'
  import BadgeLatest from './lib/BadgeLatest.svelte'
  import BadgeNext from './lib/BadgeNext.svelte'
  import TabPane from './lib/TabPane.svelte'
  import PageOverview from './lib/PageOverview.svelte'
  import PageIndicators from './lib/PageIndicators.svelte'

  const tabPanes = [
    { id: 'home', name: 'Home' },
    { id: 'indicators', name: 'Indicators' },
    {
      id: 'groups',
      name: 'Groups',
      groupList: [
        { id: 'sex', name: 'Sex' },
        { id: 'age', name: 'Age' },
        { id: 'ltUnemployed', name: 'Long-term unemployed' },
        { id: 'aboriginal', name: 'Aboriginal victorians' },
        { id: 'disability', name: 'People with disability' },
        { id: 'refugees', name: 'Refugees' }
      ]
    },
    {
      id: 'regions',
      name: 'Regions',
      groupList: [
        { id: 'vicRegions', name: 'Victorian regions' },
        { id: 'ausRegions', name: 'Australian regions' }
      ]
    },
    { id: 'industries', name: 'Industries' },
    { id: 'methodology', name: 'Methodology' },
    { id: 'disclaimer', name: 'Copyright and disclaimer' }
  ]
</script>

<Sidebar {tabPanes} />
<main>
  <div class="container py-4 px-3">
    <h1 class="my-5 display-3 text-center text-md-start">
      Victorian labour dashboard
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
    <div class="tab-content">
      <TabPane id={'home'} name={'Home'} active={true}>
        <PageOverview />
      </TabPane>
      <TabPane id={'indicators'}>
        <PageIndicators />
      </TabPane>
      {#each tabPanes.slice(2) as pane (pane.id)}
        {#if pane.hasOwnProperty('groupList')}
          {#each pane.groupList as subPane (subPane.id)}
            <TabPane name={subPane.name} id={subPane.id} />
          {/each}
        {:else}
          <TabPane name={pane.name} id={pane.id} />
        {/if}
      {/each}
    </div>
  </div>
</main>
