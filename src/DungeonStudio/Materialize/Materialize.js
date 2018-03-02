// M is in global scope

exports.initCarousel = function (elem) {
  return function () {
    var options = {
      indicators: true,
      dist: 0,
    };
    return M.Carousel.init(elem, options);
  }
}

