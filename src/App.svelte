<script>
  import { Tab } from '../node_modules/bootstrap';
  import Counter from './lib/Counter.svelte'
</script>

<main>

  <div class="container py-4 px-3 mx-auto">
    <h1>DJPR labour dashboard</h1>
    <p>
      The DJPR labour dashboard explores information on Victorian employment 
      and job seekers. Use the sidebar to navigate through information on 
      various demographics such as regions, age, sex and industry.
    </p>
    <button class="btn btn-primary">Primary button</button>
    <Counter count = {1}/>
    <Counter count = {10}/>
    <Counter count = {100}/>
  </div>


</main>

