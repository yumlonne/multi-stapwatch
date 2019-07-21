window.onload = () => {
	const app = Elm.Main.init({
        flags: JSON.parse(localStorage.getItem('savedModel'))
    });

    app.ports.saveModel.subscribe((model) => {
        localStorage.setItem('savedModel', JSON.stringify(model));
    });

};
