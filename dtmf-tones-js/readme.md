# DTMF tones using Javascript

## Contents
- [Demo](#demo)
- [Description](#description)
- [Usage](#usage)
- [Author](#author)
- [License](#license)

## Demos

See keypress-demo.html in this repository for a simple demo for playing DTMF
tones using the computer keyboard.

See ui-demo.html in this repository for a simple demo for playing DTMF
tones using the mouse.


## Description

DTMF.js is a simple Javascript module for generating DTMF (Dual Tone – Multi Frequency)
tones.  This is handy for simulating the sounds that touch-tone phones produce.

## Usage

### Including the DTMF library

Include the DTMF.js library in your HTML:

```html
<script src="js/DTMF.js"></script>
<script src="js/your-custom.js"></script>
```

### Using the DTMF library

Playing tones could not be easier!  Here's an example:

```js
DTFM.playKey('2');  // plays the DTMF tones for the telephone button "2"
```
This will play the appropriate DTMF tone.  You need to stop the tone using
```js
DTFM.stopKey('2');
```

## Author
This library is based on code by [Ifti Khan](https://www.agiletrailblazers.com/blog/modernized-technology/quick-start-to-generate-tones-in-javascript) and humbly modified by Bret Truchan.

## License
DTMF.js is open-sourced software licensed under the [MIT license](http://opensource.org/licenses/MIT)
