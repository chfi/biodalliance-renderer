# Usage

Make sure you have Purescript, Bower, and Pulp installed:
```shell
npm install -g purescript bower pulp
```

Install dependencies
```shell
bower install
```

Build a bundle like this:

```shell
pulp build --main Biodalliance.Renderer --skip-entry-point --to ./psRenderer.js
```

Then, in the file with the BioDalliance browser configuration, add the renderer script,
before the code that defines the browser, and add the renderer to the configuration:

```html
<script language="javascript" src="../build/dalliance-all.js"></script>

<script language="javascript" src="./psRenderer.js"></script>

<script language="javascript">

var PSRenderer = PS["Biodalliance.Renderer"];

var b = new Browser({
    renderers: { psrenderer: PSRenderer },
    .. /* rest of config */
})
```

The renderer can then be used in some or all tracks, by setting the `renderer` option
to `'psrenderer'` in the tier config or the browser config, respectively.
