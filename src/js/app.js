/**
 * application entry point
 */

import {initializeApp} from './mvc/controller.js';
import {parseBuild} from './util/app-util.js';

initializeApp(parseBuild(new URLSearchParams(window.location.search).get('b')));
