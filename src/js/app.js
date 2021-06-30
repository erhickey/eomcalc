/**
 * application entry point
 */

import {initializeApp} from './mvc/controller.js';
import {parseBuild} from './helpers/build-strings.js';

initializeApp(parseBuild(new URLSearchParams(window.location.search).get('b')));
