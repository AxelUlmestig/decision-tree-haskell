import React from 'react';

import SectionHeader from './sectionheader.js';
import Button from './button.js';
import ItemHeader from './itemheader.js';
import Rounded from './rounded.js';

const inputId = 'dataset_file';

const iterateDatasets = (datasets, deleteDataset) =>
    datasets.map(dataset => 
        <Rounded>
            <ItemHeader
                text={dataset.name}
                close={() => deleteDataset(dataset.name)}>
            </ItemHeader>
        </Rounded>
    )

const createUploadButton = handleFile =>
    <div>
        <input
            type='file'
            onChange={handleFileInternal(handleFile)}
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

export default props =>
    <div>
        <SectionHeader value='Datasets'></SectionHeader>
        {iterateDatasets(props.datasets, props.deleteDataset)}
        {createUploadButton(props.uploadDataset)}
    </div>
