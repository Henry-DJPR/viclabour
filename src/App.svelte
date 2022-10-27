<script>
  import svelteLogo from './assets/svelte.svg'
  import { Tab } from '../node_modules/bootstrap';
  import Counter from './lib/Counter.svelte'
</script>

<main>

  
  <div class="container py-4 px-3 mx-auto">
    <h1>DJPR labour dashboard</h1>
    <p>The DJPR labour dashboard explores information on Victorian employment and job seekers. Use the sidebar to navigate through information on various demographics such as regions, age, sex and industry.</p>
    <Counter count = {10}/>
  </div>

</main>

