var React = require('react');
var ReactDOM = require('react-dom');
var Remarkable = require('remarkable');
var request = require('superagent');


var CommentForm = React.createClass({
    getInitialState: function() {
	return {author: '', text: ''};
    },
    handleAuthorChange: function(e) {
	this.setState({author: e.target.value});
    },
    handleTextChange: function(e) {
	this.setState({text: e.target.value});
    },
    handleSubmit: function(e) {
	e.preventDefault();
	var author = this.state.author.trim();
	var text = this.state.text.trim();
	if (!text || !author) {
	    return;
	}
	this.props.onCommentSubmit({author: author, text: text});
	this.setState({author: '', text: ''});
    },
    render: function() {
	return (
	    <form className="commentForm" onSubmit={this.handleSubmit}>
	      <div>
		<input
	          type="text"
	          placeholder="Your name"
	          value={this.state.author}
	          onChange={this.handleAuthorChange}
		  />
	      </div>
	      <div>
		<textarea
	          type="text"
	          placeholder="Say something..."
	          value={this.state.text}
	          onChange={this.handleTextChange}
		  />
	      </div>
	      <input type="submit" value="Post" />
	    </form>
	);
    }
});

var Comment = React.createClass({
    rawMarkup: function() {
	var md = new Remarkable();
	var rawMarkup = md.render(this.props.children.toString());
	return { __html: rawMarkup };
    },

    render: function() {
	return (
	        <div className="comment">
		  <h2 className="commentAuthor">{this.props.author}</h2>
		  <span dangerouslySetInnerHTML={this.rawMarkup()} />
		</div>
	);
    }
});

var CommentBox = React.createClass({
    loadCommentsFromServer: function() {
	request.get(this.props.url)
	    .end(function(err,res){
		if(err || !res.ok){
		    console.error(err.toString());
		} else {
		    this.setState({data: JSON.parse(res.text)});
		}
	    }.bind(this));
    },

    handleCommentSubmit: function(comment) {
	var comments = this.state.data.reverse();
	// Optimistically set an id on the new comment. It will be replaced by an
	// id generated by the server.
	comment.id = Math.random().toString(36).replace(/[^a-zA-Z0-9]+/g, '');
	var newComments = comments.concat([comment]);
	this.setState({data: newComments});

	// console.log(comment.text);
	// console.log(comment.author);
	request
	    .post(this.props.postUrl)
	    .send({
		text: comment.text,
		author: comment.author
	    })
	    .end(function(err, res){
		if (err || !res.ok) {
		    console.error(this.props.url, err.toString());
		} else {
		    this.setState({data: JSON.parse(res.text)});
		}
	    }.bind(this));
    },

    getInitialState: function() {
	return {data: []};
    },

    componentDidMount: function() {
	this.loadCommentsFromServer();
	setInterval(this.loadCommentsFromServer, this.props.pollInterval);
    },

    render: function() {
	return (
	        <div className="commentBox">
		<h1>Comments</h1>
		<CommentForm onCommentSubmit={this.handleCommentSubmit} />
		<CommentList data={this.state.data.reverse()} />
		</div>
	);
    }

});

var CommentList = React.createClass({
    render: function() {
	var commentNodes = this.props.data.map(function(comment) {
	    return (
		<Comment author={comment.author} key={comment.id}>
		  {comment.text}
		</Comment>
	    );
	});

	return (
	    <div className="commentList">
	      {commentNodes}
	    </div>
	);

    }
});

exports.CommentBox = CommentBox

ReactDOM.render(
    <CommentBox url="/api/comments" postUrl="/api/post-comment" pollInterval={2000}/>,
    document.getElementById('content')
);