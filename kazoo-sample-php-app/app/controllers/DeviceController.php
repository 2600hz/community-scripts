<?php

class DeviceController extends BaseController {

    protected $layout = 'layouts.master';

    public function index() {
        $this->layout->content = View::make('devices.index');
    }

}