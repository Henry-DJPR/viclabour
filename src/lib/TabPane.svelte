<script>
  export let id = 'test'
  export let name = 'Test'
  export let active = false
  let paneClass = ''
  let paneWidth
  let visibleCheck = false

  if (active === true) {
    paneClass = 'tab-pane active fade show'
  } else {
    paneClass = 'tab-pane fade'
  }

  function updateVisibleCheck(w) {
    if (w > 0) {
      visibleCheck = true
    }
  }

  $: updateVisibleCheck(paneWidth)
</script>

<div
  class={paneClass}
  {id}
  role="tabpanel"
  aria-labelledby={'tab-' + id}
  bind:clientWidth={paneWidth}
>
  {#if visibleCheck === true}
    <slot>
      <h2>
        {name} tabpanel
      </h2>
    </slot>
  {/if}
</div>
