# The Nature of Sound?

Using samples to create music you quickly learn the power of manipulating, splicing, reversing & chopping, turning a fixed sample into something dynamic. This leads quickly to an understanding that samples are data. The more we understand this data the more we increase the flexibility of the sample as an instrument. That is the goal of this project. 

## Build

Only tested on Mac. Requires the compilation of c++ Xtract lib.

```
make
```

## Usage

```clojure
(require '[the-nature-of-sound.core :as nos])
(nos/peek-inside "test/fixtures/test.wav")
```

## Features of sound

Data provided by `peek-inside` fn.

`:f0`
Fundamental frequency. The musical pitch of a note that is perceived as the lowest partial present.

`:midi-note`
Midi note.

`:rms-amplitude`
Root mean squared amplitude. Volume.

`:spectral-centroid`
A good predictor of the brightness of a sound.

`:spectral-irregularity`
A good preditor of the noiseness of a sound.

`:spectral-inharmonicity`
The degree to which the frequencies of overtones depart from whole multiples of the fundamental frequency. 
* Bowed strong instruments ~ perfectly harmonic.
* Tuned percussion ~ nearly harmonic
* Untuned percussion ~ not harmonic.

`:spectral-skewness`
Whether or not the spectrum is skewed towards a particular range of values.

`:spectral-kurtosis`
A good predictor of the pointedness of a spectrum, can be used to indicate pitchiness.

# License

Copyright © 2016-present Joseph Wilk

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

For full license information, see the LICENSE file.
