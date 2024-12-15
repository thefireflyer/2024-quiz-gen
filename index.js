import { main } from "./output/Main";

MathJax = {
    tex: {
      inlineMath: [['$', '$'], ['\\(', '\\)']],
    },
    svg: {
      fontCache: 'global'
    }
};

main();