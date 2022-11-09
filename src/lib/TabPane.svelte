<script>
  export let id = 'test'
  export let name = 'Test'
  export let active = false
  let paneClass = ''

  if (active === true) {
    paneClass = 'tab-pane active fade show'
  } else {
    paneClass = 'tab-pane fade'
  }
</script>

<div class={paneClass} {id} role="tabpanel" aria-labelledby={'tab-' + id}>
  <slot>
    <h2>
      {name} tabpanel
    </h2>
  </slot>
</div>
