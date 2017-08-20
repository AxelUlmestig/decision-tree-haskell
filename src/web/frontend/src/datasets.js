import React from 'react';

const inputId = 'dataset_file';

const iterateDatasets = (datasets, deleteDataset) =>
    datasets.map(dataset =>
        <div className='deleteButtonWrapper rounded'>
            <div className='verticallyAlignedWrapper'>
                {dataset.name}
            </div>
            <img
                className='deleteButton clickable'
                src={require('./static/delete-button.png')}
                onClick={() => deleteDataset(dataset.name)}>
            </img>
        </div>
    )

const createUploadButton = handleFile =>
    <div>
        <input
            type='file'
            onChange={handleFileInternal(handleFile)}
            id={inputId}
            style={{'display': 'none'}}>
        </input>
        <div
            className='rounded clickable centeredWrapper'
            onClick={() => document.getElementById(inputId).click()}>
            Upload Dataset
        </div>
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
        <div className="headerWrapper">
            <div className="header">Datasets</div>
        </div>
        {iterateDatasets(props.datasets, props.deleteDataset)}
        {createUploadButton(props.uploadDataset)}
    </div>
