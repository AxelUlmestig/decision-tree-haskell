import React from 'react';

const inputId = 'dataset_file';

const iterateDatasets = (datasets, deleteDataset) =>
    datasets.map(dataset =>
        <li key={dataset.name}>
            {dataset.name} <button onClick={() => deleteDataset(dataset.name)}>Delete</button>
        </li>
    )

const createUploadButton = handleFile =>
    <div>
        <input type='file' id={inputId}></input><br/>
        <button onClick={handleFileInternal(handleFile)}>Upload Dataset</button>
    </div>

const handleFileInternal = handleFile => () => {
    const files = document.getElementById(inputId).files;
    if (files.length <= 0) return false;

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
        <ul>{iterateDatasets(props.datasets, props.deleteDataset)}</ul>
        {createUploadButton(props.uploadDataset)}
    </div>
