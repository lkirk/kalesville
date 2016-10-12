import React, { Component } from 'react';
import { render } from 'react-dom';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';


class App extends Component {
    handleSelect(index, last) {
	console.log('Selected tab: ' + index + ', Last tab: ' + last);
    }

    render() {
	return (
	    <Tabs
	    onSelect={this.handleSelect}
	    selectedIndex={0} // landing page is home
	    >
	      <TabList>
		<Tab>Home</Tab>
		<Tab>Recipes</Tab>
		<Tab>About</Tab>
	      </TabList>

	      <TabPanel>
		<h2>Hey there, welcome!</h2>
		<div>
		  what do you think? <a href='/comments'>click here to say something</a>
		</div>
	      </TabPanel>
	      <TabPanel>
		<h2>eat some stuff!</h2>
	      </TabPanel>
	      <TabPanel>
		<h2>what is kalesville?</h2>
	      </TabPanel>
	    </Tabs>
	);
    }
}

render( <div>
	<App/> 
	</div>,
    document.getElementById('content'));
