var React = require('react');
var ReactDOM = require('react-dom');
var Remarkable = require('remarkable');
var request = require('superagent');


var RecipeForm = React.createClass({
    getInitialState: function() {
	return {title:'', ingredients: '', procedures: ''};
    },

    handleTitleChange: function(e) {
	this.setState({title: e.target.value});
    },

    handleIngredientsChange: function(e) {
	this.setState({ingredients: e.target.value});
    },

    handleProceduresChange: function(e) {
	this.setState({procedures: e.target.value});
    },

    handleSubmit: function(e) {
	e.preventDefault();
	var title = this.state.title.trim();
	var ingredients = this.state.ingredients.trim();
	var procedures = this.state.procedures.trim();
	if (!title || !ingredients || !procedures) {
	    return;
	}
	this.props.onRecipeSubmit({
	    title: title,
	    ingredients: ingredients,
	    procedures: procedures
	});
	this.setState({
	    title: '',
	    ingredients: '',
	    procedures: ''
	});
    },

    render: function() {
	return (
	    <form className="recipeForm" onSubmit={this.handleSubmit}>
	      <div>
		<input
	          type="text"
	          placeholder="Recipe Title"
	          value={this.state.title}
	          onChange={this.handleTitleChange}
		  />
	      </div>
	      <div>
		<textarea
		  type="text"
		  placeholder="Enter ingredients here. Markdown is completely valid, see here: "
		  value={this.state.ingredients}
		  onChange={this.handleIngredientsChange}
		  />
	      </div>
	      <div>
		<textarea
		  type="text"
		  placeholder="Enter a procedure list here. Markdown is completely valid here too!"
		  value={this.state.procedures}
		  onChange={this.handleProceduresChange}
		  />
	      </div>
	      <input type="submit" />
	    </form>
	);
    }

});


var RecipeFormDisplay = React.createClass({

    handleRecipeSubmit: function(recipe) {
	request
	    .post(this.props.postUrl)
	    .send({
		title: recipe.title,
		ingredients: recipe.ingredients,
		procedures: recipe.procedures
	    })
	    .end(function(err, res){
		if (err || !res.ok) {
		    console.error(this.props.url, err.toString());
		}
	    });
    },

    render: function() {
	return (
	        <div className="recipeFormDisplay">
		  <RecipeForm onRecipeSubmit={this.handleRecipeSubmit} />
		</div>
	);
    }

});

var RecipeSelector = React.createClass({

    getInitialState: function() {
	return {
	    data: null
	}
    },

    componentDidMount: function() {
	request.get(this.props.recipeListUrl)
	.end(
	    function(err,res) {
		if(err || !res.ok) {
		    console.error(err.toString());
		} else {
		    this.setState({data: JSON.parse(res.text)});
		}
	    }.bind(this));
    },

    componentWillUnmount: function() {
	this.serverRequest.abort();
    },

    render: function() {
	if (this.state.data) {
	    return (
		<div className='recipeSelector'>
		  <button className='selectorButton'>Select a recipe to view</button>
		  <div className='buttonDropdownContent'>
		    {this.state.data.map(function(recipe) {
			return (
			    <a href={['/recipe/' + recipe.id]}>{recipe.title}</a>
			)
		    })}
		  </div>
		</div>
	    )
	} else {
	    return (
		<div className='recipeSelector'></div>
	    )
	}
    }
});

let components = (
    <div>
      <RecipeSelector recipeListUrl='/api/recipes' />
      <RecipeFormDisplay postUrl='/api/post-recipe' />
    </div>
);

ReactDOM.render(components, document.getElementById('content'));
