
import { finder } from '@medv/finder'

console.log('@medv/finder loaded');
var h3 = $("h3")[0];
console.log('h3 selector => ', finder(h3));
window.finder = finder;
