import React from 'react';
import ReactDOM from 'react-dom';
import Remarkable from 'remarkable';
import request from 'superagent';


function addCss(cssCode) {
    const styleElement = document.createElement("style");
    styleElement.type = "text/css";
    if (styleElement.styleSheet) {
	styleElement.styleSheet.cssText = cssCode;
    } else {
	styleElement.appendChild(document.createTextNode(cssCode));
    }
    document.getElementsByTagName("head")[0].appendChild(styleElement);
}

function dynamicallyAdjustAcordionHeight(selector, height, unit) {
    addCss(
	`${selector}{height:${height}${unit};}`
    );
}

class RecipeForm extends React.Component {
    constructor(props) {
	super(props);
	this.state = {title:'', ingredients: '', procedures: ''};
    }

    handleTitleChange({target}) {
	this.setState({title: target.value});
    }

    handleIngredientsChange({target}) {
	this.setState({ingredients: target.value});
    }

    handleProceduresChange({target}) {
	this.setState({procedures: target.value});
    }

    handleSubmit(e) {
	e.preventDefault();
	const title = this.state.title.trim();
	const ingredients = this.state.ingredients.trim();
	const procedures = this.state.procedures.trim();
	if (!title || !ingredients || !procedures) {
	    return;
	}
	this.props.onRecipeSubmit({
	    title,
	    ingredients,
	    procedures
	});
	this.setState({
	    title: '',
	    ingredients: '',
	    procedures: ''
	});
    }

    render() {
	return (
	    <form className="recipeForm" onSubmit={this.handleSubmit.bind(this)}>
	      <div>
		<input
		  type="text"
		  placeholder="Recipe Title"
		  value={this.state.title}
		  onChange={this.handleTitleChange.bind(this)}
		  />
	      </div>
	      <div>
		<textarea
		  className="display-linebreak"
		  type="text"
		  placeholder="Enter ingredients here. Use markdown for formatting"
		  value={this.state.ingredients}
		  onChange={this.handleIngredientsChange.bind(this)}
		  />
	      </div>
	      <div>
		<textarea
		  type="text"
		  placeholder="Enter ingredients here. Use markdown for formatting"
		  value={this.state.procedures}
		  onChange={this.handleProceduresChange.bind(this)}
		  />
	      </div>
	      <input type="submit" />
	    </form>
	);
    }

};


class RecipeFormDisplay extends React.Component {
    handleRecipeSubmit({title, ingredients, procedures}) {
	request
	.post(this.props.postUrl)
	.send({
	    title,
	    ingredients,
	    procedures
	})
	.end(function(err, {ok}) {
	    if (err || !ok) {
		console.error(this.props.url, err.toString());
	    }
	});
    }

    render() {
	return (
	    <div className="recipeFormDisplay">
	      <RecipeForm onRecipeSubmit={this.handleRecipeSubmit.bind(this)} />
	    </div>
	);
    }

};

class RecipeSelector extends React.Component {
    constructor(props) {
	super(props);
	this.state = {data: null};
    }

    getRecipeList() {
	request.get(this.props.recipeListUrl)
	.end(
	    function(err,res) {
		if(err || !res.ok) {
		    console.error(err.toString());
		} else {
		    this.setState({
			data: JSON.parse(res.text).sort(
			    function(a,b) { {/* sort by title name */}
				return a.title.localeCompare(b.title)
			    })
		    });
		}
	    }.bind(this));
    }

    componentDidMount() {
	this.getRecipeList()
    }

    componentWillUnmount() {
	this.serverRequest.abort();
    }

    render() {
	if (this.state.data) {
	    const height = String(this.state.data.length * 2 + 2.5);
	    dynamicallyAdjustAcordionHeight(
		'.vertical [type=checkbox]:checked ~ label ~ .accordionContent#accordian-recipe-list', height, 'em');
	    return (
		<div id="navcontainer">
		  <ul id="navlist">
                    {this.state.data.map(({id, title}) =>
		      <li key={id}><a href={[`/recipe/${id}`]}>{title}</a></li>)}
		  </ul>
		</div>
            );
	} else {
	    return (
		<div className='recipeSelector'></div>
	    )
	}
    }
};


let components = (
    <div>
      <div className="accordion vertical">
	<ul>
	  <li>
	    <input type="checkbox" id="checkbox-1" name="checkbox-accordion" />
	    <label htmlFor="checkbox-1">Enter A Recipe</label>
	    <div id="accordian-recipe-form" className="accordionContent">
	      <RecipeFormDisplay postUrl='/api/recipe' />
	    </div>
	  </li>
	  <li>
	    <input type="checkbox" id="checkbox-2" name="checkbox-accordion" />
	    <label htmlFor="checkbox-2">View A Recipe</label>
	    <div id="accordian-recipe-list" className="accordionContent">
	      <RecipeSelector recipeListUrl='/api/recipes' />
	    </div>
	  </li>
	</ul>
      </div>
    </div>
);

ReactDOM.render(components, document.getElementById('content'));
