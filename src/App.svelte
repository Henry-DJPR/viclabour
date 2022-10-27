<script>
  import { Tab } from '../node_modules/bootstrap';
  import Counter from './lib/Counter.svelte'
</script>

<main>

  <div class="container py-4 px-3 mx-auto">
    <h1>Hello, Bootstrap and Vite!</h1>
    <button class="btn btn-primary">Primary button</button>
    <Counter count = {1}/>
    <Counter count = {10}/>
    <Counter count = {100}/>
  </div>


</main>

