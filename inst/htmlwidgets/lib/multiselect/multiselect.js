// Varun Dewan 2019
var clEvent = new Event('passiveclick', {
  bubbles: true,
  cancelable: true,
});

var $$ = {
  get: function (el, selector) {
    var ele = el.querySelectorAll(selector);
    for (var i = 0; i < ele.length; i++) {
      this.init(ele[i]);
    }
    return ele;
  },
  template: function (html) {
    var template = document.createElement('div');
    template.innerHTML = html.trim();
    return this.init(template.childNodes[0]);
  },
  init: function (ele) {
    ele.on = function (event, func) { this.addEventListener(event, func); }
    return ele;
  }
};

// Build the plugin
var drop = function (info) {
  var o = {
    options: info.options,
    selected: info.selected || [],
    preselected: info.preselected || [],
    open: false,
    html: {
      select: $$.get(info.el, info.selector)[0],
      options: $$.get(info.el, info.selector + ' option'),
      parent: undefined,
    },
    init: function () {
      // Setup Drop HTML
      this.html.parent = $$.get(info.el, info.selector)[0].parentNode
      this.html.drop = $$.template('<div class="drop"></div>')
      this.html.dropDisplay = $$.template('<div class="drop-display">Display</div>')
      this.html.dropOptions = $$.template('<div class="drop-options">Options</div>')
      this.html.dropScreen = $$.template('<div class="drop-screen"></div>')

      this.html.parent.insertBefore(this.html.drop, this.html.select)
      this.html.drop.appendChild(this.html.dropDisplay)
      this.html.drop.appendChild(this.html.dropOptions)
      this.html.drop.appendChild(this.html.dropScreen)
      // Hide old select
      this.html.drop.appendChild(this.html.select);

      // Core Events
      var that = this;
      this.html.dropDisplay.on('click', function () { that.toggle() });
      this.html.dropScreen.on('click', function () { that.toggle() });
      // Run Render
      this.load()
      this.preselect()
      this.render();
    },
    toggle: function () {
      this.html.drop.classList.toggle('open');
    },
    currentIndices: function() {
      var cur = [];
      for (var index = 0; index < this.options.length; index++) {
        if (this.isSelected(index)) { cur.push(index); }
      }
      return cur;
    },
    addOption: function (e, element, propogate) {
      var index = Number(element.dataset.index);
      if (propogate) {
        var btnEl = document.getElementsByClassName('btnadd-' + index);
        [].forEach.call(btnEl, function(x) { x.dispatchEvent(clEvent) });
        var els = document.getElementsByClassName('dc-' + this.options[index].value);
        [].forEach.call(els, function(x) { x.classList.remove('hidden'); })
      } else {
        this.clearStates()
        this.selected.push({
          index: Number(index),
          state: 'add',
          removed: false
        })
        this.options[index].state = 'remove';
        this.render()
      }
    },
    removeOption: function (e, element, propogate) {
      e.stopPropagation();
      var index = Number(element.dataset.index);
      if (propogate) {
        var btnEl = document.getElementsByClassName('btnclose-' + index);
        [].forEach.call(btnEl, function(x) { x.dispatchEvent(clEvent) });
        var els = document.getElementsByClassName('dc-' + this.options[index].value);
        [].forEach.call(els, function(x) { x.classList.add('hidden'); });
      } else {
        this.clearStates()
        this.selected.forEach(function (select) {
          if (select.index == index && !select.removed) {
            select.removed = true
            select.state = 'remove'
          }
        })
        this.options[index].state = 'add'
        this.render();  
      }
    },
    load: function () {
      this.options = [];
      for (var i = 0; i < this.html.options.length; i++) {
        var option = this.html.options[i]
        this.options[i] = {
          html: option.innerHTML,
          value: option.value,
          selected: option.selected,
          state: ''
        }
      }
    },
    preselect: function () {
      var that = this;
      this.selected = [];
      this.preselected.forEach(function (pre) {
        that.selected.push({
          index: pre,
          state: 'add',
          removed: false
        })
        that.options[pre].state = 'remove';
      })
    },
    render: function () {
      this.renderDrop()
      this.renderOptions()
      window.cur_trscope_selected_cols = this.currentIndices();
    },
    renderDrop: function () {
      var that = this;
      var parentHTML = $$.template('<div></div>')
      this.selected.forEach(function (select, index) {
        var option = that.options[select.index];
        var childHTML = $$.template('<span class="item ' + select.state + '">' + option.html + '</span>')
        var childCloseHTML = $$.template(
          '<span class="btnclose btnclose-' + select.index + '" data-index="' + select.index + '">⊗</span>')
        childCloseHTML.on('click', function (e) { that.removeOption(e, this, true) })
        childCloseHTML.on('passiveclick', function (e) { that.removeOption(e, this, false) })
        childHTML.appendChild(childCloseHTML)
        parentHTML.appendChild(childHTML)
      })
      this.html.dropDisplay.innerHTML = '';
      this.html.dropDisplay.appendChild(parentHTML)
    },
    renderOptions: function () {
      var that = this;
      var parentHTML = $$.template('<div></div>')
      this.options.forEach(function (option, index) {
        var childHTML = $$.template(
          '<a data-index="' + index + '" class="' + option.state + ' btnadd-' + index + '">' + option.html + '</a>')
        childHTML.on('click', function (e) {
          var nn = 0;
          for (var index = 0; index < that.options.length; index++) {
            if (that.isSelected(index)) { nn++; }
          }
          if (nn < 2) {
            that.addOption(e, this, true) 
            nn++;
          }
          if (nn >= 2) {
            that.html.drop.classList.remove('open');
          }
        })
        childHTML.on('passiveclick', function (e) {
          var nn = 0;
          for (var index = 0; index < that.options.length; index++) {
            if (that.isSelected(index)) { nn++; }
          }
          if (nn < 2) {
            that.addOption(e, this, false) 
            nn++;
          }
          if (nn >= 2) {
            that.html.drop.classList.remove('open');
          }
        })
        parentHTML.appendChild(childHTML)
      })
      this.html.dropOptions.innerHTML = '';
      this.html.dropOptions.appendChild(parentHTML)
    },
    clearStates: function () {
      var that = this;
      this.selected.forEach(function (select, index) {
        select.state = that.changeState(select.state)
      })
      this.options.forEach(function (option) {
        option.state = that.changeState(option.state)
      })
    },
    changeState: function (state) {
      switch (state) {
        case 'remove':
          return 'hide'
        case 'hide':
          return 'hide'
        default:
          return ''
      }
    },
    isSelected: function (index) {
      var check = false
      this.selected.forEach(function (select) {
        if (select.index == index && select.removed == false) check = true
      })
      return check
    }
  }; o.init(); return o;
}
