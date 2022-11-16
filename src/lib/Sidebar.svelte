<script>
  import { onMount } from 'svelte'
  import Highcharts from 'highcharts'
  import { Offcanvas, Collapse } from '../../node_modules/bootstrap'
  export let tabPanes = [{ id: 'home', name: 'Home' }]
  let firstPane = tabPanes[0]
  let panes = tabPanes.slice(1)
  let bsOffcanvas

  onMount(async () => {
    bsOffcanvas = new Offcanvas('#sidebarNav')
  })

  function dismiss() {
    bsOffcanvas.hide()
  }
</script>

<div
  class="offcanvas offcanvas-start hide"
  tabindex="-1"
  id="sidebarNav"
  aria-labelledby="sidebarLabel"
  data-bs-scroll="true"
  role="navigation"
>
  <div class="offcanvas-header">
    <h5 class="offcanvas-title" id="sidebarLabel">Demographics Menu</h5>
    <button
      type="button"
      class="btn-close"
      aria-label="Close"
      data-bs-dismiss="offcanvas"
    />
  </div>
  <div class="offcanvas-body">
    <ul class="nav flex-column nav-pills my-4 fs-5" aria-orientation="vertical">
      <li class="nav-item" role="presentation">
        <a
          class="nav-link active"
          id="tab-{firstPane.id}"
          aria-current="page"
          href="#{firstPane.id}"
          data-bs-toggle="tab"
          data-bs-target="#{firstPane.id}"
          aria-controls={firstPane.id}
          aria-selected="true"
          on:click={dismiss}
        >
          {firstPane.name}
        </a>
      </li>
      {#each panes as pane (pane.id)}
        {#if pane.hasOwnProperty('groupList')}
          <li class="nav-item" role="presentation">
            <a
              class="nav-link collapse-control"
              id="tab-{pane.id}"
              role="button"
              href="#collapse-{pane.id}"
              data-bs-toggle="collapse"
              aria-expanded="false"
              aria-controls="collapse-{pane.id}"
            >
              {pane.name}
            </a>
            <ul
              class="collapse"
              style="background-color: var(--bs-gray-400);"
              id="collapse-{pane.id}"
            >
              {#each pane.groupList as subItem (subItem.id)}
                <li class="nav-item" role="presentation">
                  <a
                    class="nav-link"
                    id="tab-{subItem.id}"
                    href="#{subItem.id}"
                    data-bs-toggle="tab"
                    data-bs-target="#{subItem.id}"
                    aria-controls={subItem.id}
                    aria-selected="false"
                    on:click={dismiss}
                  >
                    {subItem.name}
                  </a>
                </li>
              {/each}
            </ul>
          </li>
        {:else}
          <li class="nav-item" role="presentation">
            <a
              class="nav-link"
              id="tab-{pane.id}"
              href="#{pane.id}"
              data-bs-toggle="tab"
              data-bs-target="#{pane.id}"
              aria-controls={pane.id}
              aria-selected="false"
              on:click={dismiss}
            >
              {pane.name}
            </a>
          </li>
        {/if}
      {/each}
    </ul>
  </div>
</div>

<style>
  li > ul > li::marker {
    content: '';
  }
  .collapse-control::before {
    display: inline-block;
    content: '\203A';
    font-size: calc(var(--bs-body-font-size) * 1.5);
    font-weight: bold;
    transition-timing-function: ease;
    transition: transform 0.35s;
    margin-right: 0.8rem;
    transform: rotate(90deg);
  }
  .collapse-control[aria-expanded='false']::before {
    transform: rotate(0deg);
  }
</style>
