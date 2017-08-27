import React from 'react';

import Button from '../misc/button.js';

const inputId = 'dataset_file';

const UploadButton = props =>
    <div>
        <input
            type='file'
            onChange={handleFileInternal(props.uploadDataset)}
            id={inputId}
            style={{'display': 'none'}}>
        </input>
        <Button
            label='Upload Dataset'
            action={() => document.getElementById(inputId).click()}>
        </Button>
    </div>

const handleFileInternal = handleFile => input => {
    console.log(input);
    const files = input.target.files;

    const fr = new FileReader();

    fr.onload = e => {
        const filename = files[0].name;
        const setname = filename.match(/(.*).json$/)[1];
        const dataset = JSON.parse(e.target.result);

        handleFile(dataset, setname);
    };
    fr.readAsText(files[0]);
}

export default UploadButton
