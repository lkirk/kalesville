var React = require('react');
var ReactDOM = require('react-dom');
var Remarkable = require('remarkable');
var request = require('superagent');

var Recipe  = React.createClass({

    getInitialState: function() {
	return {
	    data: {
		id: null,
		ingredients: null,
		procedures: null,
		title: null
	    }
	}
    },

    componentDidMount: function() {
	var re = /(\/recipe\/[0-9A-Za-z-]+)\/?$/;
	var match = re.exec(window.location.href);
	if (match){
	    var url = "/api" + match[match.length-1];
	    request.get(url)
	    .end(
		function(err,res) {
		    if(err || !res.ok) {
			console.error(err.toString());
		    } else {
			this.setState({data: JSON.parse(res.text)});
		    }
		}.bind(this));
	}
    },

    componentWillUnmount: function() {
	this.serverRequest.abort();
    },

    rawMarkup: function(stringData) {
	var md = new Remarkable('commonmark');

	if (stringData) {
	    var rawMarkup = md.render(stringData)
	} else {
	    var rawMarkup = null
	};
	return { __html: rawMarkup };
    },

    render: function() {
	console.log()
	return (
	    <div>
	    <h1>{this.state.data.title}</h1>
	    <p>Ingredients</p>
	    <div dangerouslySetInnerHTML={this.rawMarkup(this.state.data.ingredients)} />
	    <p>Procedure</p>
	    <div dangerouslySetInnerHTML={this.rawMarkup(this.state.data.procedures)} />
	    </div>
	)
    }
});


ReactDOM.render(
    <Recipe />,
    document.getElementById('content')
);