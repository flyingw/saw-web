// Generated by purs bundle 0.13.6
var PS = {};
(function($PS) {
  "use strict";
  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];                    
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS);
(function($PS) {
  "use strict";
  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];        
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS);
(function($PS) {
  "use strict";
  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS);
(function($PS) {
  "use strict";
  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];                
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (f$prime) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(f$prime(a$prime));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS);
(function($PS) {
  "use strict";
  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS);
(function(exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function($PS) {
  "use strict";
  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);
(function($PS) {
  "use strict";
  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var Data_Function = $PS["Data.Function"];
  var Data_Unit = $PS["Data.Unit"];                
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS);
(function($PS) {
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];                 
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
})(PS);
(function(exports) {
  "use strict";          

  exports.nullable = function (a, r, f) {
    return a == null ? r : f(a);
  };
})(PS["Data.Nullable"] = PS["Data.Nullable"] || {});
(function($PS) {
  "use strict";
  $PS["Data.Nullable"] = $PS["Data.Nullable"] || {};
  var exports = $PS["Data.Nullable"];
  var $foreign = $PS["Data.Nullable"];
  var Data_Maybe = $PS["Data.Maybe"];                                   
  var toMaybe = function (n) {
      return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
  };
  exports["toMaybe"] = toMaybe;
})(PS);
(function(exports) {
  "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function($PS) {
  "use strict";
  $PS["Effect"] = $PS["Effect"] || {};
  var exports = $PS["Effect"];
  var $foreign = $PS["Effect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];                    
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
})(PS);
(function(exports) {
  "use strict";

  exports.error = function (msg) {
    return new Error(msg);
  };

  exports.throwException = function (e) {
    return function () {
      throw e;
    };
  };
})(PS["Effect.Exception"] = PS["Effect.Exception"] || {});
(function($PS) {
  "use strict";
  $PS["Effect.Exception"] = $PS["Effect.Exception"] || {};
  var exports = $PS["Effect.Exception"];
  var $foreign = $PS["Effect.Exception"];
  var $$throw = function ($2) {
      return $foreign.throwException($foreign.error($2));
  };
  exports["throw"] = $$throw;
})(PS);
(function(exports) {
  /* global exports */
  "use strict";
  var React =require("react"); 

  function createClass(baseClass) {
    function bindProperty(instance, prop, value) {
      switch (prop) {
        case 'state':
        case 'render':
        case 'componentDidMount':
        case 'componentWillUnmount':
          instance[prop] = value;
          break;

        case 'componentDidCatch':
        case 'componentWillUpdate':
        case 'shouldComponentUpdate':
        case 'getSnapshotBeforeUpdate':
          instance[prop] = function (a, b) { return value(a)(b)(); };
          break;

        case 'componentDidUpdate':
          instance[prop] = function (a, b, c) { return value(a)(b)(c)(); };
          break;

        case 'unsafeComponentWillMount':
          instance['UNSAFE_componentWillMount'] = value;
          break;

        case 'unsafeComponentWillReceiveProps':
          instance['UNSAFE_componentWillReceiveProps'] = function (a) { return value(a)(); };
          break;

        case 'unsafeComponentWillUpdate':
          instance['UNSAFE_componentWillUpdate'] = function (a, b) { return value(a)(b)(); };
          break;

        default:
          throw new Error('[purescript-react] Not a component property: ' + prop);
      }
    }

    return function (displayName) {
      return function (ctrFn) {
        var Constructor = function (props) {
          baseClass.call(this, props);
          var spec = ctrFn(this)();
          for (var k in spec) {
            bindProperty(this, k, spec[k]);
          }
        };

        Constructor.displayName = displayName;
        Constructor.prototype = Object.create(baseClass.prototype);
        Constructor.prototype.constructor = Constructor;

        return Constructor;
      };
    };
  }

  function createClassWithDerivedState(classCtr) {
    return function(displayName) {
      return function(getDerivedStateFromProps) {
        return function(ctrFn) {
          var Constructor = componentImpl(displayName)(ctrFn);
          Constructor.getDerivedStateFromProps = function(a, b) { return getDerivedStateFromProps(a)(b); };
          return Constructor;
        };
      };
    };
  }

  var componentImpl = createClass(React.Component);
  exports.componentImpl = componentImpl;

  function getProps(this_) {
    return function(){
      return this_.props;
    };
  }                                            

  function setStateImpl(this_) {
    return function(state){
      return function(){
        this_.setState(state);
      };
    };
  }                                   

  function setStateWithCallbackImpl(this_) {
    return function(state){
      return function(cb){
        return function() {
          this_.setState(state, cb);
        };
      };
    };
  }                                                           

  function getState(this_) {
    return function(){
      if (!this_.state) {
        throw new Error('[purescript-react] Cannot get state within constructor');
      }
      return this_.state;
    };
  }                           

  function forceUpdateWithCallback(this_) {
    return function(cb) {
      return function() {
        this_.forceUpdate(cb);
      };
    };
  }                                                         

  function createElement(class_) {
    return function(props){
      return function(children){
        return React.createElement.apply(React, [class_, props].concat(children));
      };
    };
  }                                         
  exports.createElementTagName = createElement;

  function createLeafElement(class_) {
    return function(props) {
      return React.createElement(class_, props);
    };
  }
  exports.createLeafElementImpl = createLeafElement;

  function createElementDynamic(class_) {
    return function(props) {
      return function(children){
        return React.createElement(class_, props, children);
      };
    };
  };                                                      
  exports.createElementTagNameDynamic = createElementDynamic;

  function createContext(defaultValue) {
    var context = React.createContext(defaultValue);
    return {
      consumer: context.Consumer,
      provider: context.Provider
    };
  }
})(PS["React"] = PS["React"] || {});
(function($PS) {
  "use strict";
  $PS["React"] = $PS["React"] || {};
  var exports = $PS["React"];
  var $foreign = $PS["React"];
  var createLeafElement = function (dictReactPropFields) {
      return $foreign.createLeafElementImpl;
  };
  var component = function (dictReactComponentSpec) {
      return $foreign.componentImpl;
  };
  exports["component"] = component;
  exports["createLeafElement"] = createLeafElement;
  exports["createElementTagName"] = $foreign.createElementTagName;
  exports["createElementTagNameDynamic"] = $foreign.createElementTagNameDynamic;
})(PS);
(function(exports) {
  /* global exports */
  "use strict";
  var React =require("react"); 

  function unsafeMkProps(key) {
    return function(value){
      var result = {};
      result[key] = value;
      return result;
    };
  }                                     

  function unsafeUnfoldProps(key) {
    return function(value){
      var result = {};
      var props = {};
      props[key] = result;

      for (var subprop in value) {
        if (value.hasOwnProperty(subprop)) {
          result[subprop] = value[subprop];
        }
      }

      return props;
    };
  }                                             

  function unsafePrefixProps(prefix) {
    return function(value){
      var result = {};

      for (var prop in value) {
        if (value.hasOwnProperty(prop)) {
          result[prefix + prop] = value[prop];
        }
      }

      return result;
    };
  }                                             

  function unsafeFromPropsArray(props) {
    var result = {};

    for (var i = 0, len = props.length; i < len; i++) {
      var prop = props[i];

      for (var key in prop) {
        if (prop.hasOwnProperty(key)) {
          result[key] = prop[key];
        }
      }
    }

    return result;
  };
  exports.unsafeFromPropsArray = unsafeFromPropsArray;
})(PS["React.DOM.Props"] = PS["React.DOM.Props"] || {});
(function($PS) {
  "use strict";
  $PS["React.DOM.Props"] = $PS["React.DOM.Props"] || {};
  var exports = $PS["React.DOM.Props"];
  var $foreign = $PS["React.DOM.Props"];
  exports["unsafeFromPropsArray"] = $foreign.unsafeFromPropsArray;
})(PS);
(function(exports) {
  "use strict";

  // module Unsafe.Coerce

  exports.unsafeCoerce = function (x) {
    return x;
  };
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});
(function($PS) {
  "use strict";
  $PS["Unsafe.Coerce"] = $PS["Unsafe.Coerce"] || {};
  var exports = $PS["Unsafe.Coerce"];
  var $foreign = $PS["Unsafe.Coerce"];
  exports["unsafeCoerce"] = $foreign.unsafeCoerce;
})(PS);
(function($PS) {
  "use strict";
  $PS["React.DOM"] = $PS["React.DOM"] || {};
  var exports = $PS["React.DOM"];
  var React = $PS["React"];
  var React_DOM_Props = $PS["React.DOM.Props"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var text = Unsafe_Coerce.unsafeCoerce;  
  var mkDOM = function (dynamic) {
      return function (tag) {
          return function (props) {
              var createElement = (function () {
                  if (!dynamic) {
                      return React.createElementTagName;
                  };
                  if (dynamic) {
                      return React.createElementTagNameDynamic;
                  };
                  throw new Error("Failed pattern match at React.DOM (line 15, column 5 - line 17, column 55): " + [ dynamic.constructor.name ]);
              })();
              return createElement(tag)(React_DOM_Props.unsafeFromPropsArray(props));
          };
      };
  };                      
  var div = mkDOM(false)("div");
  exports["text"] = text;
  exports["div"] = div;
})(PS);
(function(exports) {
  /* global exports */
  "use strict";
  var ReactDOM =require("react-dom");
  var ReactDOMServer =require("react-dom/server"); 

  exports.renderImpl = function (element, container) {
    return ReactDOM.render(element, container);
  };
})(PS["ReactDOM"] = PS["ReactDOM"] || {});
(function($PS) {
  "use strict";
  $PS["ReactDOM"] = $PS["ReactDOM"] || {};
  var exports = $PS["ReactDOM"];
  var $foreign = $PS["ReactDOM"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];                                                                  
  var render = function (rEl) {
      return function (el) {
          return Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe)(function () {
              return $foreign.renderImpl(rEl, el);
          });
      };
  };
  exports["render"] = render;
})(PS);
(function(exports) {
  "use strict";

  exports._getElementById = function (id) {
    return function (node) {
      return function () {
        return node.getElementById(id);
      };
    };
  };
})(PS["Web.DOM.NonElementParentNode"] = PS["Web.DOM.NonElementParentNode"] || {});
(function($PS) {
  "use strict";
  $PS["Web.DOM.NonElementParentNode"] = $PS["Web.DOM.NonElementParentNode"] || {};
  var exports = $PS["Web.DOM.NonElementParentNode"];
  var $foreign = $PS["Web.DOM.NonElementParentNode"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];                
  var getElementById = function (eid) {
      var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
      var $1 = $foreign["_getElementById"](eid);
      return function ($2) {
          return $0($1($2));
      };
  };
  exports["getElementById"] = getElementById;
})(PS);
(function(exports) {
  /* global window */
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["Web.HTML"] = PS["Web.HTML"] || {});
(function($PS) {
  "use strict";
  $PS["Web.HTML"] = $PS["Web.HTML"] || {};
  var exports = $PS["Web.HTML"];
  var $foreign = $PS["Web.HTML"];
  exports["window"] = $foreign.window;
})(PS);
(function($PS) {
  "use strict";
  $PS["Web.HTML.HTMLDocument"] = $PS["Web.HTML.HTMLDocument"] || {};
  var exports = $PS["Web.HTML.HTMLDocument"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];     
  var toNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
  exports["toNonElementParentNode"] = toNonElementParentNode;
})(PS);
(function(exports) {
  "use strict";

  exports.document = function (window) {
    return function () {
      return window.document;
    };
  };
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});
(function($PS) {
  "use strict";
  $PS["Web.HTML.Window"] = $PS["Web.HTML.Window"] || {};
  var exports = $PS["Web.HTML.Window"];
  var $foreign = $PS["Web.HTML.Window"];
  exports["document"] = $foreign.document;
})(PS);
(function($PS) {
  "use strict";
  $PS["Web"] = $PS["Web"] || {};
  var exports = $PS["Web"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect = $PS["Effect"];
  var Effect_Exception = $PS["Effect.Exception"];
  var React = $PS["React"];
  var React_DOM = $PS["React.DOM"];
  var ReactDOM = $PS["ReactDOM"];
  var Web_DOM_NonElementParentNode = $PS["Web.DOM.NonElementParentNode"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_HTMLDocument = $PS["Web.HTML.HTMLDocument"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];                
  var webClass = (function () {
      var render$prime = function ($$this) {
          return Control_Applicative.pure(Effect.applicativeEffect)(React_DOM.div([  ])([ React_DOM.text("app") ]));
      };
      return React.component()("web")(function ($$this) {
          var state = {};
          var render = render$prime($$this);
          return Control_Applicative.pure(Effect.applicativeEffect)({
              state: state,
              render: render
          });
      });
  })();
  var byId = function (id) {
      return function __do() {
          var doc = Control_Bind.bind(Effect.bindEffect)(Web_HTML.window)(Web_HTML_Window.document)();
          var d = Web_HTML_HTMLDocument.toNonElementParentNode(doc);
          var v = Web_DOM_NonElementParentNode.getElementById(id)(d)();
          if (v instanceof Data_Maybe.Just) {
              return v.value0;
          };
          if (v instanceof Data_Maybe.Nothing) {
              return Effect_Exception["throw"]("not found=" + id)();
          };
          throw new Error("Failed pattern match at Web (line 44, column 3 - line 46, column 40): " + [ v.constructor.name ]);
      };
  };
  var view = function __do() {
      var container = byId("container")();
      var element = React.createLeafElement()(webClass)({});
      return Data_Functor["void"](Effect.functorEffect)(ReactDOM.render(element)(container))();
  };
  exports["view"] = view;
  exports["webClass"] = webClass;
  exports["byId"] = byId;
})(PS);