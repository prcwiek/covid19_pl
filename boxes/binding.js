// binding.js

var boxxyBinding = new Shiny.OutputBinding();

$.extend(boxxyBinding, {
  find: function(scope) {
    return $(scope).find(".boxxy");
  },
  renderValue: function(el, data) {
    
    let boxValue, boxTitle;

    if(data.animate) {
      Shiny.renderDependencies(data.deps);  
      // counter 
      var counter = new CountUp(el.id + '-boxxy-value', 0, data.value);
      counter.start();
    } else {
      boxValue = document.getElementById(el.id + '-boxxy-value');
      boxValue.innerText = data.value;
    }
    
    // insert the title
    boxTitle = document.getElementById(el.id + '-boxxy-title');
    boxTitle.innerText = data.title;

    // background color
    el.style.backgroundColor = data.color;
    
  }
});

// register
Shiny.outputBindings.register(boxxyBinding, "pawel.boxxy");


