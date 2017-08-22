import React from 'react';
import ReactDOM from 'react-dom';
import Remarkable from 'remarkable';
import request from 'superagent';

class Recipe extends React.Component {
    constructor(props) {
	super(props);
	this.state = {
	    data: {
		id: null,
		ingredients: null,
		procedures: null,
		title: null
	    }
	};
    }

    componentDidMount() {
	const re = /(\/recipe\/[0-9A-Za-z-]+)\/?$/;
	const match = re.exec(window.location.href);
	if (match){
	    const url = `/api${match[match.length-1]}`;
	    request.get(url)
	    .end(
		(err, {ok, text}) => {
		    if(err || !ok) {
			console.error(err.toString());
		    } else {
			this.setState({data: JSON.parse(text)});
		    }
		});
	}
    }

    componentWillUnmount() {
	this.serverRequest.abort();
    }

    rawMarkup(stringData) {
	const md = new Remarkable();

	if (stringData) {
	    var rawMarkup = md.render(stringData)
	} else {
	    var rawMarkup = null
	};
	return { __html: rawMarkup };
    }

    render() {
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
};


ReactDOM.render(
    <Recipe />,
    document.getElementById('content')
);
